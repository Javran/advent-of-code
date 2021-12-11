{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2019.IntCode
  ( ParameterMode (..)
  , separateOpCode
  , VmResult (..)
  , Result
  , runProgram
  , startProgram
  )
where

import Control.Monad.RWS.Strict
import Control.Monad.State.Strict
import qualified Data.DList as DL
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

data ParameterMode
  = Position
  | Immediate
  | Relative
  deriving (Show)

separateOpCode :: Int -> (Int, (ParameterMode, ParameterMode, ParameterMode))
separateOpCode v = (op, (pm p1, pm p2, pm p3))
  where
    pm = \case
      0 -> Position
      1 -> Immediate
      2 -> Relative
      x -> error $ "Unknown parameter mode: " <> show x

    (v0, op) = v `quotRem` 100
    (v1, p1) = v0 `quotRem` 10
    (p3, p2) = v1 `quotRem` 10

data VmResult m r
  = Done r
  | NeedInput (Int -> m (VmResult m r))
  | SentOutput Int (m (VmResult m r))
  deriving (Functor)

type Result = VmResult IO

runVmResult :: VM (VmResult VM a) -> VmState -> IO (VmResult IO a)
runVmResult r s0 = do
  (r0, s1) <- runStateT r s0
  case r0 of
    Done v -> pure $ Done v
    NeedInput k -> pure $ NeedInput \input -> do
      (a, s2) <- runStateT (k input) s1
      runVmResult (pure a) s2
    SentOutput o k -> pure $ SentOutput o (runVmResult k s1)

-- TODO: see if we can do this with monad-control.

startProgram :: VU.Vector Int -> IO (VmResult IO (VU.Vector Int))
startProgram initMem = do
  s <- initiate initMem
  (r0, s') <- runStateT startProgramAux s
  runVmResult (pure r0) s'

data VmState = VmState
  { vmsMem :: VUM.IOVector Int
  , vmsRelBase :: Int
  }

initiate :: VU.Vector Int -> IO VmState
initiate initMem = do
  vmsMem <- VU.thaw initMem
  pure VmState {vmsMem, vmsRelBase = 0}

type VM = StateT VmState IO

startProgramAux :: VM (VmResult VM (VU.Vector Int))
startProgramAux = do
  let readAddr i = do
        mem <- gets vmsMem
        lift $ VUM.read @IO mem i
      getNum i = \case
        Position -> do
          readAddr i
        Immediate ->
          pure i
        Relative -> do
          VmState {vmsMem, vmsRelBase} <- get
          lift $ VUM.read @IO vmsMem (vmsRelBase + i)
      putNum i v = \case
        Position -> do
          mem <- gets vmsMem
          lift $ VUM.write @IO mem i v
        Immediate ->
          error "target position cannot be immediate"
        Relative -> do
          VmState {vmsMem, vmsRelBase} <- get
          lift $ VUM.write @IO vmsMem (vmsRelBase + i) v
      runAt :: Int -> VM (VmResult VM (VU.Vector Int))
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
            mem <- gets vmsMem
            finalMem <- VU.unsafeFreeze mem
            pure $ Done finalMem
          1 -> performBin (+)
          2 -> performBin (*)
          3 ->
            pure $
              NeedInput $ \input ->
                (do
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
          9 -> do
            rand1 <- readAddr (pc + 1)
            v1 <- getNum rand1 pm1
            modify (\vms -> vms {vmsRelBase = vmsRelBase vms + v1})
            runAt (pc + 2)
          _ -> error "Something went wrong"
  runAt 0

type IntCodeVm = RWST () (DL.DList Int) [Int]

runProgram :: VU.Vector Int -> [Int] -> IO (VU.Vector Int, [Int])
runProgram initMem inputs = do
  (a, _s, w) <- runRWST (runProgramAux initMem) () inputs
  pure (a, DL.toList w)

runProgramAux :: VU.Vector Int -> IntCodeVm IO (VU.Vector Int)
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
