{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Explain where

import Parser
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import Data.Char (intToDigit)
import Data.Maybe (mapMaybe)
import Data.List (group, sort, intercalate)
import Evaluate
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
    in "Repeat " ++ wrapped ++ " until the end of the row"

  Repeat n exprList ->
    let inner = concatWithComma (map explainExpr exprList)
        wrapped = if length exprList > 1 then "*" ++ inner ++ "*" else inner
    in if n == 1
       then wrapped
       else "Repeat " ++ show n ++ " times: " ++ wrapped ++ ""

  RepeatNeg n exprList ->
    let inner = concatWithComma (map explainExpr exprList)
        wrapped = if length exprList > 1 then "*" ++ inner ++ "*" else inner
    in "Repeat "++ wrapped ++  " until " ++ show n ++ " sts remains"


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
        let innerExpl = explainExprRows pat
            explSize  = length innerExpl

            numberOfRows = case ra of
              Times x       -> x * explSize
              Until x       -> x
              Centimeters x ->
                let repRows = getRowRepetitionNumber maybeGauge ra
                in round (fromIntegral repRows / fromIntegral explSize) * explSize

            reps = case ra of 
              Times x       -> x 
              Until x       -> round (fromIntegral numberOfRows / fromIntegral explSize)
              Centimeters x -> round (fromIntegral (getRowRepetitionNumber maybeGauge ra) / fromIntegral explSize)

            cms = case ra of 
              Times x       -> getCmRepetitionNumber numberOfRows maybeGauge
              Until x       -> getCmRepetitionNumber numberOfRows maybeGauge
              Centimeters x -> round x

            rowRange = "Row " ++ show n ++ "-" ++ show (n + numberOfRows - 1)
            labeledLines = zipWith (\c (Line s) -> c : ": " ++ s) ['A'..] innerExpl
            labelRange =
              if null innerExpl then ""
              else "Repeat rows [" ++ ['A'] ++ "-" ++ [toEnum (fromEnum 'A' + explSize - 1)] ++ "] "
                   ++ show reps ++ " times (" ++ show numberOfRows ++ ") (" ++ show cms ++ "cms)"

            header = rowRange ++ ": " ++ labelRange
            blockExplanation = Block (header : labeledLines)
        in blockExplanation : go (n + numberOfRows) exprs

      _ ->
        let (Line s) = explainExprRow expr
            header = "Row " ++ show n ++ ": " ++ s
        in Line header : go (n + 1) exprs

    explanationToLines :: Explanation -> [String]
    explanationToLines (Line s) = [s]
    explanationToLines (Block ss) = ss


explainCompactPattern :: Pattern -> [Explanation]
explainCompactPattern pat = case runEval 40 pat of
  Left err -> [Line $ "Error: " ++ show err]
  Right evaluated ->
    zipWith explainRow [1..] evaluated
  where
    explainRow :: Int -> [Stitch] -> Explanation
    explainRow rowNum stitches = 
      let parts = filter (not . null) $ compress stitches
      in Line $ "Row " ++ show rowNum ++ ": " ++ concatWithComma parts

    compress :: [Stitch] -> [String]
    compress [] = []
    compress (s:ss) = go 1 s ss
      where
        go n curr [] = [format n curr]
        go n curr (x:xs)
          | x == curr = go (n+1) curr xs
          | otherwise = format n curr : go 1 x xs

        format n O  = "("++ show n ++ " in needle)"  -- Si el stitch es O, devuelve cadena vacía
        format n WT = stitchToLongStr WT
        format n s =
          let desc = stitchToLongStr s
          in if n == 1 
               then desc ++ " stitch"
               else desc ++ " " ++ show n ++ " stitches"



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
  Until n -> n
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
