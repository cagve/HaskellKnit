{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Evaluate where

import Control.Monad (replicateM_, void, when)
import Control.Monad.Except (runExcept, throwError)
import Control.Monad.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.State (StateT, evalStateT)
import GHC.Generics (Generic)
import Parser
import Text.Parsec


type ExpectedSts = Int
type RowNumber = Int
type EvalEnv = (RowNumber, ExpectedSts, Int, Maybe Gauge) -- (total esperado, posición actual)
type EvalM a = StateT EvalEnv (Except String) a

  

evalExprM :: Expr -> EvalM [Stitch]
evalExprM e = case e of
  Single s -> do
    (row, total, pos, g) <- get
    let advance = stConsume s
    put (row, total, pos + advance, g)
    return [s]
  RepeatNeg n exprs -> do
    (row, total, pos, g) <- get
    sts <- concat <$> mapM evalExprM exprs
    let lenSts = length sts
    let remaining = total - pos
    let reps = if lenSts == 0 then 0 else remaining `div` lenSts
    let repsRemaining = reps - n
    when (repsRemaining `mod` lenSts /= 0) $
      throwError $
        "Error: Row(" ++ show row ++ ") >> Not divisible:  Quedan (" ++ show repsRemaining ++ "sts) y la repitición es de (" ++ show lenSts ++ "sts)."
    put (row, total, pos + repsRemaining, g)
    return $ concat (replicate repsRemaining sts)
  Repeat n exprs -> do
    sts <- concat <$> mapM evalExprM exprs
    let len = length sts
    replicateM_ (n - 1) $ do
      (row, total, pos, g) <- get
      put (row, total, pos + len, g)
    -- traceShow ("[evalRowM@]", row, total, pos) $ return ()
    return $ concat (replicate n sts)
  Zero exprs -> do
    (row, total, pos, g) <- get
    let remaining = total - pos
    sts <- concat <$> mapM evalExprM exprs
    let lenSts = length sts
    let reps = if lenSts == 0 then 0 else remaining `div` lenSts
    put (row, total, pos + reps * lenSts, g)
    return $ concat (replicate reps sts)
  RepeatBlock int pattern ->
    throwError "RepeatBlock no puede evaluarse como una expresión individual en evalExprM"

positionOfWT :: [Stitch] -> Int
positionOfWT = go 0
  where
    go _ [] = 0
    go i (s : ss)
      | s == WT = i
      | otherwise = go (i + stConsume s) ss


evalRowM :: Row -> EvalM [Stitch]
evalRowM exprs = concat <$> mapM evalExprM exprs

evalRepeatBlock :: RepeatAmount -> [Row] -> EvalM [[Stitch]]
evalRepeatBlock ra pattern = case ra of
  Times n ->
      if n <= 0
        then return []
        else do
          result <- evalPatternState pattern
          rest <- evalRepeatBlock (Times (n - 1)) pattern
          return (result ++ rest)
  Centimeters cm ->
      if cm <= 0 
        then return []
        else do
        (_, _, _, maybeGauge) <- get
        case maybeGauge of 
          Nothing -> throwError "Not gauge provided."
          Just (Gauge (Measure _ measureRowsCm) (StitchTension _ tensionRows)) -> do
            result <- evalPatternState pattern
            let oneBlockRows = length result
                oneBlockCm = if tensionRows == 0
                               then 0
                               else fromIntegral oneBlockRows
                                    / fromIntegral tensionRows
                                    * fromIntegral measureRowsCm
            if oneBlockCm <= 0
              then throwError "No se puede calcular la longitud en cm del bloque."
              else repeatUntilCm cm oneBlockCm result pattern


evalPatternState :: [Row] -> EvalM [[Stitch]]
evalPatternState [] = return []
evalPatternState (r : rs) = do
  case r of
    [RepeatBlock n innerPattern] -> do
      repeatedRows <- evalRepeatBlock n innerPattern
      restResult <- evalPatternState rs
      return (repeatedRows ++ restResult)
    _ -> do
      (row, total, pos, g) <- get
      -- \| Previous was a WT
      let newRowSt = if pos == 0 then r else replicate pos (Single O) ++ r
      when (pos > 0) $ put (row, total, 0, g)
      (row, total, pos, g) <- get
      firstRow <- evalRowM newRowSt
      let consumedSts = sum (map stConsume firstRow)

      when (row > 0 && consumedSts /= total) $
        throwError $
          "Error: Row("
            ++ show row
            ++ ") >> Expected sts: ("
            ++ show total
            ++ ") /= CurrentSts: ("
            ++ show consumedSts
            ++ ")"

      -- Calcular nuevos valores para el estado
      let newExpected = sum (map stWeight firstRow)
      let wtPos = (if WT `elem` firstRow then positionOfWT firstRow else total)
      let newPos = total - wtPos
      put (row + 1, newExpected, newPos, g)
      restResult <- evalPatternState rs
      return (firstRow : restResult)

stWeight :: Stitch -> Int
stWeight WT = 0
stWeight _ = 1

stConsume :: Stitch -> Int
stConsume SSK = 2
stConsume S2KP2 = 3
stConsume YO = 0
stConsume WT = 0
stConsume M1L = 0
stConsume M1R = 0
stConsume CO = 0
stConsume (KTOG n) = n
stConsume _ = 1

countStsInRow :: [Stitch] -> Int
countStsInRow sts = do
  let n = sum (map stWeight sts)
  n

runEval :: ExpectedSts -> Pattern -> Either String [[Stitch]]
runEval total pat = do
  let instruction = patInstructions pat
      g = patGauge pat
  runExcept $ evalStateT (evalPatternState instruction) (0, total, 0, g)

calculateRowSize :: Int -> Gauge -> [[Stitch]] -> Measure
calculateRowSize row (Gauge (Measure w h) (StitchTension s1 s2)) evaluated = Measure w1 h2
  where
    stsInRow = countStsInRow (evaluated !! row)
    w1 = (w * stsInRow) `div` s1
    h2 = (h * row + 1) `div` s2

repeatUntilCm :: Double -> Double -> [[Stitch]] -> [Row] -> EvalM [[Stitch]]
repeatUntilCm target blockSize first pattern = go blockSize [first]
  where
    go total acc
      | total >= target = return (concat (reverse acc))
      | otherwise = do
          next <- evalPatternState pattern
          go (total + blockSize) (next : acc)



