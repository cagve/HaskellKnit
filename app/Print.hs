{-# LANGUAGE FlexibleInstances #-}
module Print where
import Parser
import Explain
import Data.Char ( intToDigit, isSpace )

reverseList  [] = []
reverseList  xs = last xs : reverseList (init xs)

calculateSep :: String -> Int
calculateSep str =
  let maxLength = 5
      nSpaces = (maxLength + 1) - length str
  in nSpaces


-- Pretty show para Stitch
class (Show a) => DebugShow a where
  dshow :: a -> String
  dshow = show

class (Show a) => PrettyShow a where
  pshow :: a -> String
  pshow = show

instance DebugShow Stitch where
  dshow = pshow

instance PrettyShow Stitch where
  pshow stitch =
    let str = stitchToStr stitch
        pad = calculateSep str
    in str++ replicate pad ' '

instance DebugShow Expr where
  dshow = pshow

instance PrettyShow Expr where
  pshow (Single s) = pshow s
  pshow (Repeat n exprs) = show n ++ "(" ++ concatMap pshow exprs ++ ")"
  pshow (Zero exprs) = "0(" ++ concatMap show exprs ++ ")"

instance DebugShow Row where
  dshow = pshow

instance DebugShow [Stitch] where
  dshow = pshow

instance PrettyShow Row where
  pshow = concatMap pshow

instance PrettyShow [Stitch] where
  pshow = concatMap pshow

instance DebugShow Pattern where
  dshow xs = unlines $ zipWith (\n line -> show n ++ ": " ++ line) [1 ..] (map pshow xs)
instance DebugShow [[Stitch]] where
  dshow rows = unlines $ zipWith formatRow [0..] rows
    where
      formatRow :: Int -> [Stitch] -> String
      formatRow i sts = 
        "(Row " ++ show i ++ ". " ++ show (countStsInRow sts) ++ " sts-> [" ++ concatMap stitchToStr sts ++ "]"


instance PrettyShow Pattern where
  pshow = unlines . map pshow
instance PrettyShow [[Stitch]] where
  pshow rows =
    let processedRows = zipWith processRow [0..] rows
        renderedRows = map pshow processedRows
        maxWidth = maximum (map length renderedRows)
        rowCount = length rows
        numberedRows = zip3 [1..] processedRows renderedRows

        formatRow (n, row, line) =
          let nSts = length row
              padding = (maxWidth - length line) `div` 2
          in replicate padding ' ' ++ line

    in unlines (map formatRow numberedRows)

instance PrettyShow Cluster where
  pshow (st, n) = "Teje "++ show st++" "++ show n ++ " veces."
instance PrettyShow [Cluster] where
  pshow = unlines . map pshow
instance PrettyShow [[Cluster]] where
  pshow = unlines . map pshow

centerRow :: Int -> [Stitch] -> String
centerRow width row =
  let rowStr = concatMap stitchToStr row
      sep = calculateSep rowStr
  in replicate sep ' ' ++ rowStr

processRow :: Int -> [Stitch] -> [Stitch]
processRow idx row
  | even idx  = reverse row  
  | otherwise = row         
