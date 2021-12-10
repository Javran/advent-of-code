{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2019.IntCode
  ( ParameterMode (..)
  , separateOpCode
  , Result (..)
  , runProgram
  , startProgram
  )
where

import Control.Monad.RWS.Strict
import qualified Data.DList as DL
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

data ParameterMode
  = Position
  | Immediate
  deriving (Show)

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

data Result input output r
  = Done r
  | NeedInput (input -> IO (Result input output r))
  | SentOutput output (IO (Result input output r))

startProgram :: V.Vector Int -> IO (Result Int Int (V.Vector Int))
startProgram initMem = do
  mem <- V.thaw initMem
  let readAddr i = VM.unsafeRead @IO mem i
      getNum i = \case
        Position -> do
          readAddr i
        Immediate ->
          pure i
      putNum i v = \case
        Position -> do
          VM.unsafeWrite @IO mem i v
        Immediate ->
          error "target position cannot be immediate"
      runAt :: Int -> IO (Result Int Int (V.Vector Int))
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
            condJump expectZero = do
              rand1 <- readAddr (pc + 1)
              v1 <- getNum rand1 pm1
              rand2 <- readAddr (pc + 2)
              v2 <- getNum rand2 pm2
              if (if expectZero then v1 == 0 else v1 /= 0)
                then runAt v2
                else runAt (pc + 3)

        case opCode of
          99 -> do
            finalMem <- V.unsafeFreeze mem
            pure $ Done finalMem
          1 -> performBin (+)
          2 -> performBin (*)
          3 -> do
            pure $
              NeedInput
                (\input -> do
                   rand1 <- readAddr (pc + 1)
                   putNum rand1 input pm1
                   runAt (pc + 2))
          4 -> do
            rand1 <- readAddr (pc + 1)
            out <- getNum rand1 pm1
            pure $ SentOutput out $ runAt (pc + 2)
          5 -> condJump False
          6 -> condJump True
          7 -> performBin (\x y -> if x < y then 1 else 0)
          8 -> performBin (\x y -> if x == y then 1 else 0)
          _ -> error "Something went wrong"
  runAt 0

type IntCodeVm = RWST () (DL.DList Int) [Int]

runProgram :: V.Vector Int -> [Int] -> IO (V.Vector Int, [Int])
runProgram initMem inputs = do
  (a, _s, w) <- runRWST (runProgramAux initMem) () inputs
  pure (a, DL.toList w)

runProgramAux :: V.Vector Int -> IntCodeVm IO (V.Vector Int)
runProgramAux initMem = do
  r <- lift $ startProgram initMem
  let drive = \case
        Done v -> pure v
        NeedInput k -> do
          i <- state (\xs -> (head xs, tail xs))
          r' <- liftIO $ k i
          drive r'
        SentOutput out k -> do
          tell (DL.singleton out)
          r' <- liftIO k
          drive r'
  drive r
