{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


module Parser where
import Control.Monad (replicateM_, void, when)
import Control.Monad.Except (runExcept, throwError)
import Control.Monad.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.State (StateT, evalStateT)
import Data.Aeson (FromJSON, ToJSON (toJSON), eitherDecode, object)
import Data.Char (intToDigit, isSpace)
import Data.List
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace (trace, traceShow)
import GHC.Generics (Generic)
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Aeson.Types ((.=))

-- | DATA TYPES

data Pattern = Pattern
  { patTitle :: String,
    patGauge :: Maybe Gauge,
    patInstructions :: [Row]
  } deriving (Generic, Show, Eq)

instance ToJSON Pattern where
  toJSON (Pattern title mgauge instructions) =
    object
      [ "title" .= title,
        "gauge" .= fmap show mgauge,
        "instructions" .= map rowToList instructions
      ]

data Measure = Measure Int Int deriving (Generic, Show, Eq, Ord)
instance ToJSON Measure

data StitchTension = StitchTension Int Int deriving (Generic, Show, Eq, Ord)
instance ToJSON StitchTension

data Gauge = Gauge
  { measureGauge :: Measure,
    stitchGauge :: StitchTension
  }
  deriving (Generic, Show, Eq)
instance ToJSON Gauge

data RepeatAmount
  = Times Int
  | Centimeters Double
  | Until Int
  deriving (Generic, Show, Eq, Ord)
instance ToJSON RepeatAmount

data Stitch = CO | K | P | YO | SSK | S2KP2 | KTOG Int | M1R | M1L | WT | O | C Int deriving (Generic, Show, Eq, Ord)
instance ToJSON Stitch

data Expr
  = Single Stitch
  | Repeat Int [Expr]
  | RepeatNeg Int [Expr]
  | RepeatBlock RepeatAmount [Row]
  | Zero [Expr]
  deriving (Generic, Show, Eq, Ord)
instance ToJSON Expr

type Row = [Expr]

-- | Parsing expresions
comment :: Parser ()
comment = try $ do
  _ <- string "//"
  _ <- manyTill anyChar newline
  return ()

repeatAmount :: Parser RepeatAmount
repeatAmount = try cmParser <|> timesParser <|> untilParser
  where
    -- repeat(cm(12.5)) => Centimeters 12.5
    cmParser = do
      string "cm"
      char '('
      amt <- float
      char ')'
      return $ Centimeters amt
    timesParser = Times <$> number
    untilParser = do 
      string "until"
      char '('
      amt <- number
      char ')'
      return $ Until amt

gaugeParser :: String -> Either ParseError Gauge
gaugeParser = parse gaugeParser ""
  where
    gaugeParser = do
      _ <- string "gauge"
      _ <- char '('
      cols <- number
      _ <- char ','
      rows <- number
      _ <- char ')'
      return $ Gauge (Measure 10 10) (StitchTension cols rows)

ktogParser :: Parser Expr
ktogParser = try $ do
  _ <- char 'k'
  n <- number
  _ <- string "tog"
  return (Single (KTOG n))

wtParser :: Parser Expr
wtParser = try $ do
  _ <- string "wt"
  void $ char '('
  n <- number
  void $ char ')'
  return $ Repeat 1 (Single WT : replicate n (Single O))

number :: Parser Int
number = read <$> many1 digit


-- Float parser
float :: Parser Double
float = do
  whole <- many1 digit
  char '.'
  frac <- many1 digit
  return $ read (whole ++ "." ++ frac)

colorParser :: Parser Expr
colorParser = try $ do
  _ <- char 'x'
  n <- number
  return $ Single (C n)

castonParser :: Parser Expr
castonParser = try $ do
  _ <- string "co"
  void $ char '('
  n <- number
  void $ char ')'
  return $ Repeat 1 (replicate n (Single CO))

stitchExpr :: Parser Expr
stitchExpr =
  try castonParser
    <|> try colorParser
    <|> try (char 'k' >> return (Single K))
    <|> try (char 'p' >> return (Single P))
    <|> try ktogParser
    <|> try wtParser
    <|> try (string "m1r" >> return (Single M1R))
    <|> try (string "m1l" >> return (Single M1L))
    <|> try (string "s2kp2" >> return (Single S2KP2))
    <|> try (string "ssk" >> return (Single SSK))
    <|> try (string "yo" >> return (Single YO))

repeatNegExpr :: Parser Expr
repeatNegExpr = do
  void $ char '-'
  n <- number
  void $ char '('
  exprs <- sepEndBy expr skipSpaceOrComment
  void $ char ')'
  return $ RepeatNeg n exprs

repeatExpr :: Parser Expr
repeatExpr = do
  n <- number
  void $ char '('
  exprs <- sepEndBy expr skipSpaceOrComment
  void $ char ')'
  return $ Repeat n exprs

zeroExpr :: Parser Expr
zeroExpr = do
  void $ char '0'
  void $ char '('
  exprs <- sepEndBy expr skipSpaceOrComment
  void $ char ')'
  return $ Zero exprs

-- NUEVO PARSER: RepeatBlock repeat()
repeatBlockExpr :: Parser Expr
repeatBlockExpr = try $ do
  string "repeat"
  char '('
  amt <- repeatAmount
  char ')'
  char '{'
  expr <- repeatBlockParserExpr
  char '}'
  return $ RepeatBlock amt expr


repeatBlockParserExpr :: Parser [Row]
repeatBlockParserExpr = do
  skipSpaceOrComment
  rows <- row `sepEndBy1` (char ';' >> skipSpaceOrComment)
  skipSpaceOrComment
  return rows

expr :: Parser Expr
expr = try repeatBlockExpr <|> try zeroExpr <|>  try repeatNegExpr <|> try repeatExpr <|> stitchExpr

row :: Parser Row
row =
  try
    ( do
        rb <- repeatBlockExpr
        return [rb]
    )
    <|> sepEndBy1 expr skipSpaceOrComment

instructions :: Parser [Row]
instructions = do
  skipSpaceOrComment
  rows <- row `sepEndBy1` (char ';' >> skipSpaceOrComment)
  skipSpaceOrComment
  eof
  return rows

parsePattern :: String -> Either ParseError [Row]
parsePattern = parse instructions ""

parsePatternFile filePath = do
  content <- readFile filePath
  return $ parseFile content

parseFile :: String -> Either ParseError Pattern
parseFile content = do
  let ls = lines content
      (gaugeLines, patternLines) = partition (isPrefixOf "gauge") ls
      gaugeStr = unlines gaugeLines
      patternStr = unlines patternLines
  mgauge <- case gaugeLines of
    [] -> Right Nothing
    _ -> Just <$> gaugeParser gaugeStr
  pat <- parsePattern patternStr
  return Pattern {patTitle="DEFAULT", patGauge=mgauge, patInstructions=pat}



-- | AUXILIAR FUNCTIONS
skipSpaceOrComment :: Parser ()
skipSpaceOrComment = skipMany (space *> pure () <|> comment <|> newline *> pure ())

isWT :: Expr -> Bool
isWT (Single WT) = True
isWT _ = False


isCO :: Expr -> Bool
isCO (Single CO) = True
isCO _ = False

isColor :: Stitch -> Bool
isColor (C n) = True
isColor _ = False

isColorPattern :: [[Stitch]] -> Bool
isColorPattern pattern = do
  let flat = concat pattern
  all isColor flat

stitchToLongStr :: Stitch -> String
stitchToLongStr K = "Knit"
stitchToLongStr P = "Purl"
stitchToLongStr YO = "Yarn over"
stitchToLongStr M1L = "Make 1 left"
stitchToLongStr M1R = "Make 1 right"
stitchToLongStr WT = "Wrap and turn"
stitchToLongStr O = ""
stitchToLongStr SSK = "Slip Slip Knit"
stitchToLongStr CO = "Cast on"
stitchToLongStr S2KP2 = "Slip 2, knit 1 and pass slipped stitchExpr over"
stitchToLongStr (KTOG n) = "Knit " ++ [intToDigit n] ++ " together"
stitchToLongStr (C n) = "Color" ++ [intToDigit (n + 1)]

stitchToStr :: Stitch -> String
stitchToStr K = "K"
stitchToStr P = "P"
stitchToStr YO = "YO"
stitchToStr M1L = "M1L"
stitchToStr M1R = "M1R"
stitchToStr WT = "WT"
stitchToStr O = "_"
stitchToStr SSK = "SSK"
stitchToStr CO = "CO"
stitchToStr S2KP2 = "S2KP2"
stitchToStr (KTOG n) = "K" ++ [intToDigit n] ++ "TOG"
stitchToStr (C n) = "C" ++ [intToDigit n]


exprToStr :: Expr -> String
exprToStr (Single s) = stitchToStr s
exprToStr _ = ""

rowToStr :: Row -> String
rowToStr exprs = unwords (map exprToStr exprs)

rowToList :: Row -> [String]
rowToList = map exprToStr

