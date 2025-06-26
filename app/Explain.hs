{-# LANGUAGE FlexibleInstances #-}
module Explain where

import Parser
import Data.Char (intToDigit)
import Data.List (group)
import Data.Maybe (mapMaybe)

-- | Agrupación de un stitch con su cantidad consecutiva
type Cluster = (Stitch, Int)

-- | Agrupación de una expresión con su cantidad consecutiva
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
explainClusterPattern :: [[Stitch]] -> String
explainClusterPattern = unlines . zipWith explainClusterRow [1..]

-- === Explicación de expresiones ===

-- | Explicación extendida de un stitch
explainStitch :: Stitch -> String
explainStitch = stitchToStr

-- | Explica una expresión en formato lista de líneas para mejor legibilidad
explainExpr :: Expr -> [String]
explainExpr expr = case expr of
  Repeat 1 (firstExpr : tailExprs)
    | isWT firstExpr ->
        let tailSize = length tailExprs
        in ["Wrap and turn. German rows. (" ++ show tailSize ++ " sts. in needle)."]
  Repeat 1 (firstExpr : tailExprs)
    | isCO firstExpr ->
        let tailSize = length tailExprs + 1
        in ["Cast on " ++ show tailSize ++ " sts."]
  Single st -> case st of
    K -> ["K1"]
    P -> ["P1"]
    _ -> [explainStitch st ++ "."]
  Repeat n exprList ->
    let inner = concatWithComma (map (concat . explainExpr) exprList)
        wrapped = if length exprList > 1 then "*" ++ inner ++ "*" else inner
    in ["Repeat " ++ show n ++ " times: " ++ wrapped ++ "."]
  RepeatNeg n exprList ->
    let inner = concatWithComma (map (concat . explainExpr) exprList)
        wrapped = if length exprList > 1 then "*" ++ inner ++ "*" else inner
    in ["Repeat " ++ wrapped ++ " until " ++ show n ++ " stitch" ++ (if n > 1 then "es" else "") ++ " remain."]
  Zero exprList ->
    let inner = concatWithComma (map (concat . explainExpr) exprList)
        wrapped = if length exprList > 1 then "*" ++ inner ++ "*" else inner
    in ["Repeat " ++ wrapped ++ " until the end of the row."]
  RepeatBlock n pat ->
    let innerLines = concatMap explainExprRow pat
        header = "Work following for " ++ show n ++ " row" ++ (if n > 1 then "s" else "") ++ ":"
    in header : innerLines
  _ ->
    ["(pattern element not recognized)."]

-- | Explica clusters de expresiones, omitiendo "Single O"
explainClusterExpr :: [ClusterExpr] -> [String]
explainClusterExpr = mapMaybe explainOne
  where
    explainOne :: ClusterExpr -> Maybe String
    explainOne (st, _n) = case st of
      Single O -> Nothing
      _        -> Just (concat (explainExpr st))

-- | Explica una fila de expresiones agrupadas en clusters
explainClusterExprRow :: Int -> [Expr] -> String
explainClusterExprRow _n row = concat (explainClusterExpr (clusterExpr row))

explainExprRow :: [Expr] -> [String]
explainExprRow = concatMap explainExpr

explainExprPattern :: Pattern -> String
explainExprPattern pat = unlines $ zipWith explainRow [1..] pat
  where
    explainRow :: Int -> [Expr] -> String
    explainRow _n row = concat (explainClusterExpr (clusterExpr row))

