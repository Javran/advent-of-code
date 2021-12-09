{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Javran.AdventOfCode.Y2019.Day5
  (
  )
where


import Control.Applicative
import Control.Monad
import Control.Monad.RWS.Strict
import Control.Monad.ST
import Control.Monad.State.Strict
import Control.Monad.Writer
import Data.Bifunctor
import Data.Bool
import Data.Char
import qualified Data.DList as DL
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Debug.Trace
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day5 deriving (Generic)

data ParameterMode = Position | Immediate deriving (Show)

separateOpCode :: Int -> (Int, (ParameterMode, ParameterMode, ParameterMode))
separateOpCode v = (op, (pm p1, pm p2, pm p3))
  where
    pm = \case
      0 -> Position
      1 -> Immediate
      x -> error $ "Unknown parameter mode: " <> show x

    (v0, op) = v `quotRem` 100
    (v1, p1) = v0 `quotRem` 10
    (p3, p2) = v1 `quotRem` 10

type IntCodeVmT = RWST () (DL.DList Int) [Int]

runProgram :: V.Vector Int -> [Int] -> (V.Vector Int, [Int])
runProgram initMem inputs = runST do
  (a, _s, w) <- runRWST runProgram' () inputs
  pure (a, DL.toList w)
  where
    runProgram' :: forall s. IntCodeVmT (ST s) (V.Vector Int)
    runProgram' = do
      mem <- lift $ V.thaw initMem
      let readAddr i = lift $ VM.unsafeRead @(ST s) mem i
          getNum i = \case
            Position -> do
              readAddr i
            Immediate ->
              pure i
          putNum i v = \case
            Position -> do
              lift $ VM.unsafeWrite @(ST s) mem i v
            Immediate ->
              error "target position cannot be immediate"
          runAt pc = do
            opRaw <- readAddr pc
            let (opCode, (pm1, pm2, pm3)) = separateOpCode opRaw
                performBin op = do
                  rand1 <- readAddr (pc + 1)
                  v1 <- getNum rand1 pm1
                  rand2 <- readAddr (pc + 2)
                  v2 <- getNum rand2 pm2
                  dst <- readAddr (pc + 3)
                  putNum dst (op v1 v2) pm3
                  runAt (pc + 4)
            case opCode of
              99 -> pure ()
              1 ->
                performBin (+)
              2 -> do
                performBin (*)
              3 -> do
                inp <- gets head
                modify tail
                rand1 <- readAddr (pc + 1)
                putNum rand1 inp pm1
                runAt (pc + 2)
              4 -> do
                rand1 <- readAddr (pc + 1)
                out <- getNum rand1 pm1
                tell (DL.singleton out)
                runAt (pc + 2)
              _ -> error "Something went wrong"
      runAt 0
      finalMem <- lift $ V.unsafeFreeze mem
      pure finalMem

instance Solution Day5 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow, answerS} = do
    (extraOps, rawInput) <- consumeExtraLeadingLines <$> getInputS
    let xs = fmap (read @Int) . splitOn "," . head . lines $ rawInput
        mem = V.fromList xs
    case extraOps of
      Nothing -> do
        -- running with login example
        let (_afterMem, logs) = runProgram mem [1]
        answerS "Part 1:"
        mapM_ answerShow logs
      Just rawTestInputs -> do
        let inputs :: [Int]
            inputs = fmap read . filter ((/= "#"). take 1) $ rawTestInputs
            (_afterMem, logs) = runProgram mem inputs
        mapM_ answerShow logs
