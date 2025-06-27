{-# LANGUAGE FlexibleInstances #-}
module Explain where

import Parser
import Data.Char (intToDigit)
import Data.List (group, intercalate)
import Data.Maybe (mapMaybe)

type Cluster = (Stitch, Int)
type ClusterExpr = (Expr, Int)

-- | Concatena una lista de strings separados por comas
concatWithComma :: [String] -> String
concatWithComma = foldr1 (\a b -> a ++ ", " ++ b)

-- | Agrupa una lista de stitches en clusters consecutivos
cluster :: [Stitch] -> [Cluster]
cluster = map (\g -> (head g, length g)) . group

-- | Agrupa una lista de expresiones en clusters consecutivos
clusterExpr :: [Expr] -> [ClusterExpr]
clusterExpr = map (\g -> (head g, length g)) . group

-- | Explica una lista de clusters de stitches, ignorando los "O"
explainCluster :: [Cluster] -> [String]
explainCluster = mapMaybe explainOne
  where
    explainOne :: Cluster -> Maybe String
    explainOne (st, n) = case st of
      O -> Nothing
      _ -> Just (stitchToLongStr st ++ " " ++ show n ++ " times. ")

-- | Explica una fila de stitches agrupada en clusters
explainClusterRow :: Int -> [Stitch] -> String
explainClusterRow _n row = concat (explainCluster (cluster row))

-- | Explica un patrón completo de filas de stitches
explainClusterPattern :: [[Stitch]] -> [String]
explainClusterPattern = zipWith explainClusterRow [1..]

-- === Explicación de expresiones ===
data Explanation 
  = Line String
  | Block [String]
  deriving (Show, Eq)

-- | Explicación extendida de un stitch
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
    _ -> explainStitch st ++ "."

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


-- | DEUELVE LINEA
explainExprRow :: [Expr] -> Explanation
explainExprRow exprs = Line $ intercalate ", " (map explainExpr exprs)

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
