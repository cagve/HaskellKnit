{-# LANGUAGE FlexibleContexts #-}

module Parser where
import Debug.Trace (trace, traceShow)
import Control.Monad (replicateM_, void, when)
import Control.Monad.Except (runExcept, throwError)
import Control.Monad.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.State (StateT, evalStateT)
import Data.Char ( intToDigit, isSpace )
import Debug.Trace (trace)
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T


debug :: String -> a -> a
debug _ x = x 



parsePatternFile :: FilePath -> IO (Either ParseError Pattern)
parsePatternFile filePath = do
  content <- readFile filePath
  let nonBlankLines = filter (not . all isSpace) (lines content)
      inputWithSpaces = concat nonBlankLines
      input = filter (not . isSpace) inputWithSpaces -- quita espacios, tabs, etc
  return $ parsePattern input


data Stitch = CO | K | P | YO | SSK |S2KP2 | KTOG Int | M1R | M1L | WT | O | C Int deriving (Show, Eq) 

data Expr
  = Single Stitch
  | Repeat Int [Expr]
  | RepeatNeg Int [Expr]
  | RepeatBlock Int Pattern
  | Zero [Expr]
  deriving (Show, Eq)

type Row = [Expr]
type Pattern = [Row]

-- Parser
ktogParser :: Parser Expr
ktogParser = try $ do
  _ <- char 'k'
  n <- many1 digit
  _ <- string "tog"
  return (Single (KTOG (read n)))


wtParser :: Parser Expr
wtParser = try $ do
  _ <- string "wt"
  void $ char '('
  nStr <- many1 digit
  void $ char ')'
  let n = read nStr
  return $ Repeat 1 (Single WT : replicate n (Single O))

number :: Parser Int
number = read <$> many1 digit

colorParser :: Parser Expr
colorParser = try $ do
  _ <- char 'x'
  n <- many1 digit
  return (Single (C (read n)))

castonParser :: Parser Expr
castonParser = try $ do
  _ <- string "co"
  void $ char '('
  n <- number
  void $ char ')'
  return $ Repeat 1 (replicate n (Single CO))

stitch :: Parser Expr
stitch =
    try (castonParser)
    <|> try (colorParser)
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
  exprs <- many expr
  void $ char ')'
  return $ RepeatNeg n exprs

repeatExpr :: Parser Expr
repeatExpr = do
  n <- number
  void $ char '('
  exprs <- many expr
  void $ char ')'
  return $ Repeat n exprs

zeroExpr :: Parser Expr
zeroExpr = do
  void $ char '0'
  void $ char '('
  exprs <- many expr
  void $ char ')'
  return $ Zero exprs


-- NUEVO PARSER: RepeatBlock repeat()
repeatBlockParser :: Parser Expr
repeatBlockParser = try $ do
  void $ string "repeat"
  void $ char '('
  n <- number
  void $ char ')'
  void $ char '{'
  expr <- patternParser  
  void $ char '}'
  return $ RepeatBlock n expr


expr :: Parser Expr
expr = try repeatBlockParser <|> try zeroExpr <|> try repeatExpr <|> try repeatNegExpr <|> stitch

row :: Parser Row
row = try (do
            rb <- repeatBlockParser
            return [rb])
     <|> many1 expr

patternParser :: Parser Pattern
patternParser = row `sepEndBy1` (char ';')

parsePattern :: String -> Either ParseError Pattern
parsePattern = parse (patternParser <* eof) ""

parsePatternString :: String -> Either ParseError Pattern
parsePatternString str =
  let nonBlankLines = filter (not . all isSpace) (lines str)
      inputWithSpaces = concat nonBlankLines
      input = filter (not . isSpace) inputWithSpaces  -- Elimina espacios, tabs, etc.
  in parsePattern input

-- Evaluador

type ExpectedSts = Int
type RowNumber = Int
type EvalEnv = (RowNumber, ExpectedSts, Int) -- (total esperado, posición actual)
type EvalM a = StateT EvalEnv (Except String) a

evalExprM :: Expr -> EvalM [Stitch]
evalExprM e = case e of
  Single s -> do
    (row, total, pos) <- get
    let advance = stConsume s
    put (row, total, pos + advance)
    return [s]
  RepeatNeg n exprs -> do
    (row, total, pos) <- get
    sts <- concat <$> mapM evalExprM exprs
    let lenSts = length sts
    let remaining = total - pos
    let reps = if lenSts == 0 then 0 else remaining `div` lenSts
    let repsRemaining = reps - n
    when (repsRemaining `mod` lenSts /= 0) $
      throwError $
        "Error: Row(" ++ show row ++ ") >> Not divisible:  Quedan (" ++ show repsRemaining ++ "sts) y la repitición es de (" ++ show lenSts ++ "sts)."
    put (row, total, pos + repsRemaining)
    return $ concat (replicate repsRemaining sts)
  Repeat n exprs -> do
    sts <- concat <$> mapM evalExprM exprs
    let len = length sts
    replicateM_ (n - 1) $ do
      (row, total, pos) <- get
      put (row, total, pos + len)
      -- traceShow ("[evalRowM@]", row, total, pos) $ return ()
    return $ concat (replicate n sts)
  Zero exprs -> do
    (row, total, pos) <- get
    let remaining = total - pos
    sts <- concat <$> mapM evalExprM exprs
    let lenSts = length sts
    let reps = if lenSts == 0 then 0 else remaining `div` lenSts
    put (row, total, pos + reps * lenSts)
    return $ concat (replicate reps sts)
  RepeatBlock int pattern -> 
    throwError "RepeatBlock no puede evaluarse como una expresión individual en evalExprM"


positionOfWT :: [Stitch] -> Int
positionOfWT  = go 0 
  where
    go _ [] = trace "[DEBUG] WT no encontrado" 0
    go i (s:ss)
      | s == WT   = i
      | otherwise = go (i + stConsume s) ss
      -- | s == WT   = traceShow ("[DEBUG] WT encontrado en posicion logica", i) i
      -- | otherwise = traceShow ("[DEBUG] Avanzando: stitch =", s) (go (i + stitchAdvance s) ss)



evalRowM :: Row -> EvalM [Stitch]
evalRowM exprs = concat <$> mapM evalExprM exprs

evalRepeatBlock :: Int -> Pattern -> EvalM [[Stitch]]
evalRepeatBlock 0 _ = return []
evalRepeatBlock n pattern = do
  result <- evalPatternState pattern
  rest <- evalRepeatBlock (n - 1) pattern
  return (result ++ rest)

evalPatternState :: Pattern -> EvalM [[Stitch]]
evalPatternState [] = return []
evalPatternState (r:rs) = do
  case r of
    [RepeatBlock n innerPattern] -> do
      repeatedRows <- evalRepeatBlock n innerPattern
      restResult <- evalPatternState rs
      return (repeatedRows ++ restResult)
    
    _ -> do  
      (row, total, pos) <- get
      -- | Previous was a WT
      let newRowSt = if pos == 0 then r else replicate pos (Single O) ++ r
      when (pos > 0) $ put (row, total, 0)
      (row, total, pos) <- get 
      firstRow <- evalRowM newRowSt
      let consumedSts = sum (map stConsume firstRow)

      when (row > 0 && consumedSts /= total) $
        throwError $
          "Error: Row(" ++ show row ++ ") >> Expected sts: (" ++ show total ++ 
          ") /= CurrentSts: (" ++ show consumedSts ++ ")"
      
      -- Calcular nuevos valores para el estado
      let newExpected = sum (map stWeight firstRow)
      let wtPos = case WT `elem` firstRow of
                True  -> positionOfWT firstRow
                False -> total 
      let newPos = total - wtPos
      put (row + 1, newExpected, newPos)
      restResult <- evalPatternState rs
      return (firstRow : restResult)

stWeight:: Stitch -> Int
stWeight WT = 0
stWeight _ = 1

stConsume :: Stitch -> Int
stConsume SSK      = 2
stConsume S2KP2    = 3
stConsume YO       = 0
stConsume WT       = 0
stConsume M1L      = 0
stConsume M1R      = 0
stConsume CO       = 0
stConsume (KTOG n) = n
stConsume _        = 1

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
stitchToLongStr S2KP2 = "Slip 2, knit 1 and pass slipped stitch over"
stitchToLongStr (KTOG n) = "Knit " ++ [intToDigit n] ++ " together"
stitchToLongStr (C n) = "Color 1" ++ [intToDigit n] 

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


countStsInRow :: [Stitch] -> Int
countStsInRow sts = do
  let n = sum (map stWeight sts)
  n

runEval :: ExpectedSts -> Pattern -> Either String [[Stitch]]
runEval total pat = runExcept $ evalStateT (evalPatternState pat) (0, total, 0)

convertPattern :: [[Stitch]] -> [[Text]]
convertPattern rows = map (map (T.pack . stitchToStr)) rows



