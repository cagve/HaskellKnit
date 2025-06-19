{-# LANGUAGE FlexibleInstances #-}
module Explain where
import Parser 
import Data.Char (intToDigit)
import Data.List (group)
import Data.Maybe (mapMaybe)


type Cluster = (Stitch, Int)
type ClusterExpr = (Expr, Int)

concatWithComma :: [String] -> String
concatWithComma = foldr1 (\a b -> a ++ ", " ++ b)


-- ||| CLUSTER EXPLAINING
cluster :: [Stitch] -> [Cluster]
cluster xs = map (\g -> (head g, length g)) (group xs)

clusterExpr :: [Expr] -> [ClusterExpr]
clusterExpr xs = map (\g -> (head g, length g)) (group xs)

explainCluster :: [Cluster] -> [String]
explainCluster clusters = mapMaybe explainOne clusters
  where 
    explainOne :: Cluster -> Maybe String
    explainOne (st, n) = case st of 
      O -> Nothing
      _ -> Just(stitchToLongStr st ++ " " ++ show n ++ " times. ")


explainClusterRow :: Int -> [Stitch] -> String
explainClusterRow n row =
  -- "Row " ++ show n ++ ": \n" ++
  concat (explainCluster (cluster row))
  -- "ℹ️ En la aguja hay " ++ show (countStsInRow row) ++ " puntos.\n"

explainClusterPattern :: [[Stitch]] -> String
explainClusterPattern stsRows = unlines $ zipWith explainClusterRow [1..] stsRows

---- |||| EXPR
explainStitch :: Stitch -> String
explainStitch = stitchToStr

-- Explicar una expresión en formato frase corta, para agrupar por fila
explainExpr :: Expr -> String
explainExpr expr = case expr of
  Repeat 1 (firstExpr : tailExprs) 
    | isWT firstExpr -> 
         let tailSize = length tailExprs
         in "Wrap and turn. German rows. (" ++ show tailSize++" sts. in needle)"
  Repeat 1 (firstExpr : tailExprs ) 
    | isCO firstExpr -> 
         let tailSize = length tailExprs+1 -- | CUENTA LOS QUE QEUEDA Y LE AñADE EL PRIMERO
         in "Cast on " ++ show tailSize++" sts."
  Single st ->
    explainStitch st
  Repeat n exprList ->
    "Repeat " ++ show n ++ " times (" ++ concatWithComma (map explainExpr exprList) ++ ")"
  RepeatNeg n exprList ->
    "K until " ++ show n ++ " stitch" ++ (if n > 1 then "es" else "") ++ " remain, " ++ concatWithComma (map explainExpr exprList)
  Zero exprList ->
    "Repeat to end of row (" ++ concatWithComma (map explainExpr exprList) ++ ")"
  RepeatBlock n pat ->
    "Work following for " ++ show n ++ " row" ++ (if n > 1 then "s" else "") ++ " (" ++ concatWithComma (map explainExprRow pat) ++ ")"
  _ ->
    "(pattern element not recognized)"


explainClusterExpr :: [ClusterExpr] -> [String]
explainClusterExpr clusters = mapMaybe explainOne clusters
  where 
    explainOne :: ClusterExpr -> Maybe String
    explainOne (st, n) = case st of 
      Single O -> Nothing
      _ -> Just((explainExpr st) ++ " " ++ show n ++ " sts. ")

explainClusterExprRow :: Int -> [Expr] -> String
explainClusterExprRow n row =
  concat (explainClusterExpr (clusterExpr row))

explainExprRow :: [Expr] -> String
explainExprRow row = concatWithComma (map explainExpr row)


-- Explicar patrón completo con filas numeradas y encabezado "FILA X:"
explainExprPattern :: Pattern -> String
explainExprPattern pat = unlines $ zipWith explainRow [1..] pat
  where
    explainRow :: Int -> [Expr] -> String
    -- explainRow n row = "FILA " ++ show n ++ ": " ++ explainExprRow row
    explainRow n row = explainClusterExprRow n row
