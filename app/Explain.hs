{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Explain where

import Parser
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import Data.Char (intToDigit)
import Data.Maybe (mapMaybe)
import Data.List (group, sort, intercalate)
import qualified Data.Map as M


type Cluster = (Stitch, Int)
type ClusterExpr = (Expr, Int)

-- | Concatena una lista de strings separados por comas
concatWithComma :: [String] -> String
concatWithComma = foldr1 (\a b -> a ++ ", " ++ b)

-- === Explicación de expresiones ===
data Explanation 
  = Line String
  | Block [String]
  deriving (Generic, Show, Eq)
instance ToJSON Explanation

explainStitch :: Stitch -> String
explainStitch = stitchToStr

-- | Explica una expresión en formato lista de líneas para mejor legibilidad
explainExpr :: Expr -> String
explainExpr expr = case expr of
  RepeatBlock _ _ ->  ""
  Repeat 1 (firstExpr : tailExprs)
    | isWT firstExpr ->
        let tailSize = length tailExprs
        in "Wrap and turn. German rows. (" ++ show tailSize ++ " sts. in needle)."

  Repeat 1 (firstExpr : tailExprs)
    | isCO firstExpr ->
        let tailSize = length tailExprs + 1
        in "Cast on " ++ show tailSize ++ " sts."

  Single st -> case st of
    K -> "K1"
    P -> "P1"
    _ -> explainStitch st 

  Zero exprList ->
    let inner = concatWithComma (map explainExpr exprList)
        wrapped = if length exprList > 1 then "*" ++ inner ++ "*" else inner
    in "Repeat " ++ wrapped ++ " until the end of the row."

  Repeat n exprList -> case exprList of
      [Single st] -> case st of
        K -> "K" ++ show n
        P -> "P" ++ show n
        _ -> explainStitch st ++ " " ++ show n ++ "."
      _ ->
        let inner = concatWithComma (map explainExpr exprList)
            wrapped = "*" ++ inner ++ "*"
        in "Repeat " ++ show n ++ " times: " ++ wrapped ++ "."

  RepeatNeg n exprList -> case exprList of 
      [Single st] -> case st of
        K -> "K" ++ show n
        P -> "P" ++ show n
        _ -> explainStitch st ++ " " ++ show n ++ "."
      _ ->
        let inner = concatWithComma (map explainExpr exprList)
            wrapped = "*" ++ inner ++ "*"
        in "Repeat " ++ show n ++ " times: " ++ wrapped ++ "."


explainExprRow :: [Expr] -> Explanation
explainExprRow exprs = Line $ intercalate ", " (map explainExpr exprs)


explainCompactRow :: [Expr] -> Explanation
explainCompactRow exprs = Line $ unwords $ map encodeGroup (group exprs)
  where
    encodeGroup g  
      | length g == 1 = explainExpr (head g) 
      | otherwise = explainExpr (head g) ++ "x" ++ show (length g) 

explainCompactRows :: [Row] -> [Explanation]
explainCompactRows = map explainCompactRow

explainExprRows :: [Row] -> [Explanation]
explainExprRows = map explainExprRow


explainExprPattern :: Pattern -> [Explanation]
explainExprPattern (Pattern _ maybeGauge instructions) = go 1 instructions
  where
    go :: Int -> [Row] -> [Explanation]
    go _ [] = []
    go n (expr:exprs) = case expr of
       [RepeatBlock ra pat] -> 
        let innerExpl = explainExprRows pat  -- [Explanation]
            explSize  = length innerExpl
            labeledLines = zipWith (\c (Line s) -> c : ": " ++ s) ['A'..] innerExpl
            header = case ra of
              Times x ->
                let numberOfRows = getRowRepetitionNumber  maybeGauge ra * explSize
                    cms = getCmRepetitionNumber numberOfRows maybeGauge 
                    rowRange = "Row " ++ show n ++ "-" ++ show (n + numberOfRows)
                    labelRange = case innerExpl of
                      [] -> ""
                      _  -> "Repeat rows [" ++ ['A'] ++ "-" ++ [toEnum (fromEnum 'A' + explSize - 1)] ++ "] " ++ show x ++ " times (" ++ show cms ++"cms)"
                in rowRange ++ ": " ++ labelRange
              Centimeters x -> 
                let repRows = getRowRepetitionNumber maybeGauge ra
                    reps = round (fromIntegral repRows / fromIntegral explSize)
                    rowRange = "Row " ++ show n ++ "-" ++ show (n + reps*explSize)
                    labelRange = case innerExpl of
                      [] -> ""
                      _  -> "Repeat rows [" ++ ['A'] ++ "-" ++ [toEnum (fromEnum 'A' + explSize - 1)] ++ "] " ++ show reps ++ " times (" ++ show x ++"cms)"
                in rowRange ++ ": " ++ labelRange
            blockExplanation = Block (header : labeledLines)
        in blockExplanation : go (n + 1) exprs      

       _ -> 
        let (Line s) = explainExprRow expr
            header = "Row " ++ show n ++ ": " ++ s
          in Line header : go (n+1) exprs

    explanationToLines :: Explanation -> [String]
    explanationToLines (Line s) = [s]
    explanationToLines (Block ss) = ss


explainCompactPattern :: Pattern -> [Explanation]
explainCompactPattern (Pattern _ maybeGauge instructions) = go 1 instructions
  where
    go :: Int -> [Row] -> [Explanation]
    go _ [] = []
    go n (expr:exprs) = case expr of
       [RepeatBlock ra pat] -> 
        let innerExpl = explainCompactRows pat  -- [Explanation]
            explSize  = length innerExpl
            labeledLines = zipWith (\c (Line s) -> c : ": " ++ s) ['A'..] innerExpl
            header = case ra of
              Times x ->
                let numberOfRows = getRowRepetitionNumber  maybeGauge ra * explSize
                    cms = getCmRepetitionNumber numberOfRows maybeGauge 
                    rowRange = "Row " ++ show n ++ "-" ++ show (n + numberOfRows)
                    labelRange = case innerExpl of
                      [] -> ""
                      _  -> "Repeat rows [" ++ ['A'] ++ "-" ++ [toEnum (fromEnum 'A' + explSize - 1)] ++ "] " ++ show x ++ " times (" ++ show cms ++"cms)"
                in rowRange ++ ": " ++ labelRange
              Centimeters x -> 
                let repRows = getRowRepetitionNumber maybeGauge ra
                    reps = round (fromIntegral repRows / fromIntegral explSize)
                    rowRange = "Row " ++ show n ++ "-" ++ show (n + reps*explSize)
                    labelRange = case innerExpl of
                      [] -> ""
                      _  -> "Repeat rows [" ++ ['A'] ++ "-" ++ [toEnum (fromEnum 'A' + explSize - 1)] ++ "] " ++ show reps ++ " times (" ++ show x ++"cms)"
                in rowRange ++ ": " ++ labelRange
            blockExplanation = Block (header : labeledLines)
        in blockExplanation : go (n + 1) exprs      

       _ -> 
        let (Line s) = explainCompactRow expr
            header = "Row " ++ show n ++ ": " ++ s
          in Line header : go (n+1) exprs

    explanationToLines :: Explanation -> [String]
    explanationToLines (Line s) = [s]
    explanationToLines (Block ss) = ss


-- Esta función toma todas las líneas del bloque y genera un resumen (puedes ajustarla a gusto)
summarizeBlock :: [String] -> String
summarizeBlock linesInBlock =
  if allEqual linesInBlock
    then head linesInBlock
    else intercalate " /n " linesInBlock

-- Utilidad para ver si todos los elementos son iguales
allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (== x) xs


getRowRepetitionNumber :: Maybe Gauge -> RepeatAmount -> Int
getRowRepetitionNumber mg ra = case ra of 
  Times n -> n
  Centimeters c -> case mg of
          Just (Gauge (Measure cmWidth cmHeight) (StitchTension stWidth stHeight)) ->
              let stHeight' = fromIntegral stHeight :: Double
                  cmHeight' = fromIntegral cmHeight :: Double
                  rowTimes = round (c * stHeight' / cmHeight') 
              in rowTimes
          Nothing -> 0

-- DEVUELVE LOS CENTIMETROS
getCmRepetitionNumber :: Int -> Maybe Gauge -> Int
getCmRepetitionNumber n mg  = case mg of
    Just (Gauge (Measure cmWidth cmHeight) (StitchTension stWidth stHeight)) ->
      let
        n' = fromIntegral n :: Double
        cmHeight' = fromIntegral cmHeight :: Double
        stHeight' = fromIntegral stHeight :: Double
        cmValue = round (n' * cmHeight' / stHeight')
      in cmValue
    _ -> 0

countExprs :: [Expr] -> M.Map Expr Int
countExprs = foldr (\e -> M.insertWith (+) e 1) M.empty
