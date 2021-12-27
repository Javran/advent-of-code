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
  , parseCodeOrDie
  , startProgramFromFoldable
  , communicate
  )
where

{-
  Feature-complete IntCode interpreter.

  Test coverage for all opcodes:

  - Y2019 Day2
  - Y2019 Day5
  - Y2019 Day7
  - Y2019 Day9

 -}

import Control.Monad.RWS.Strict
import Control.Monad.State.Strict
import qualified Data.DList as DL
import Data.Foldable
import qualified Data.IntMap as IM
import Data.List
import qualified Data.List.Ordered as LOrd
import Data.List.Split
import Data.Maybe
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

runVmResult :: VM (VmResult VM a) -> VmState -> IO (Result a)
runVmResult r s0 = do
  (r0, s1) <- runStateT r s0
  case r0 of
    Done v -> pure $ Done v
    NeedInput k -> pure $ NeedInput \input -> do
      (a, s2) <- runStateT (k input) s1
      runVmResult (pure a) s2
    SentOutput o k -> pure $ SentOutput o (runVmResult k s1)

startProgram :: VU.Vector Int -> IO (Result (VU.Vector Int))
startProgram initMem = do
  s <- initiate initMem
  (r0, s') <- runStateT startProgramAux s
  runVmResult (pure r0) s'

startProgramFromFoldable :: Foldable f => f Int -> IO (Result (VU.Vector Int))
startProgramFromFoldable xs = do
  s <- initiateFromFoldable xs
  (r0, s') <- runStateT startProgramAux s
  runVmResult (pure r0) s'

{-
  Send the program a sequence of inputs, and collect an expected
  amount of outputs.
 -}
communicate :: [Int] -> Int -> IO (Result a) -> IO ([Int], IO (Result a))
communicate inputs outputCount k0 = case inputs of
  [] ->
    fix
      (\loop curK outCnt acc ->
         if outCnt > 0
           then do
             r0 <- curK
             case r0 of
               SentOutput o k1 -> do
                 loop
                   k1
                   (outCnt -1)
                   (acc <> DL.singleton o)
               _ ->
                 error $ "expected to collect " <> show outCnt <> " more outputs"
           else pure (DL.toList acc, curK))
      k0
      outputCount
      DL.empty
  x : xs -> do
    r0 <- k0
    case r0 of
      NeedInput k -> do
        r1 <- k x
        communicate xs outputCount (pure r1)
      _ ->
        error $ "expected to need " <> show (length inputs) <> " more inputs."

data VmState = VmState
  { vmsMem :: VUM.IOVector Int
  , vmsRelBase :: Int
  , vmsSparse :: IM.IntMap Int
  }

{-
  Defines the maximum space vector is allowed to occupy.

  - allowed vector indices are: [0..maxVectorSize-1]
  - vmsSparse stores any index >= maxVectorSize
  - vector growth on write demands.

 -}
maxVectorSize :: Int
maxVectorSize = 0xFFFF

findGrowTarget :: Int -> Int -> Int
findGrowTarget curSize i = head $ filter (\sz -> i < sz) sizes
  where
    sizes = LOrd.union (iterate (* 2) curSize) [maxVectorSize]

initiate :: VU.Vector Int -> IO VmState
initiate initMem = do
  vmsMem <- VU.thaw initMem
  pure VmState {vmsMem, vmsRelBase = 0, vmsSparse = IM.empty}

initiateFromFoldable :: Foldable f => f Int -> IO VmState
initiateFromFoldable xs = do
  vmsMem <- VU.unsafeThaw (VU.fromList (toList xs))
  pure VmState {vmsMem, vmsRelBase = 0, vmsSparse = IM.empty}

type VM = StateT VmState IO

startProgramAux :: VM (VmResult VM (VU.Vector Int))
startProgramAux = do
  let readAddr i =
        if i >= maxVectorSize
          then do
            m <- gets ((IM.!? i) . vmsSparse)
            pure $ fromMaybe 0 m
          else do
            mem <- gets vmsMem
            let sz = VUM.length mem
            if i >= sz
              then pure 0
              else lift $ VUM.read @IO mem i
      writeAddr i v =
        if i >= maxVectorSize
          then modify (\vms -> vms {vmsSparse = IM.insert i v (vmsSparse vms)})
          else do
            mem <- do
              m' <- gets vmsMem
              let curSize = VUM.length m'
              if i < curSize
                then pure m'
                else do
                  let targetSize = findGrowTarget curSize i
                  newM <- lift $ VUM.grow m' (targetSize - curSize)
                  modify (\vms -> vms {vmsMem = newM})
                  pure newM
            lift $ VUM.write @IO mem i v
      getNum i = \case
        Position -> do
          readAddr i
        Immediate ->
          pure i
        Relative -> do
          VmState {vmsRelBase} <- get
          readAddr (vmsRelBase + i)
      putNum i v = \case
        Position -> writeAddr i v
        Immediate ->
          error "target position cannot be immediate"
        Relative -> do
          VmState {vmsRelBase} <- get
          writeAddr (vmsRelBase + i) v
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

runProgram :: Foldable f => f Int -> [Int] -> IO (VU.Vector Int, [Int])
runProgram initMem inputs = do
  (a, _s, w) <- runRWST (runProgramAux initMem) () inputs
  pure (a, DL.toList w)

runProgramAux :: Foldable f => f Int -> IntCodeVm IO (VU.Vector Int)
runProgramAux xs = do
  r <- lift $ startProgramFromFoldable xs
  let drive = \case
        Done v -> pure v
        NeedInput k -> do
          i <- state (\ys -> (head ys, tail ys))
          r' <- liftIO $ k i
          drive r'
        SentOutput out k -> do
          tell (DL.singleton out)
          r' <- liftIO k
          drive r'
  drive r

{-
  lines starting with `#` are ignored in first pass,
  then remaining contents are unlined to form a comma-separated list of ints for parsing.
 -}
parseCodeOrDie :: String -> [Int]
parseCodeOrDie =
  fmap (read @Int)
    . splitOn ","
    . unlines
    . filter (not . ("#" `isPrefixOf`))
    . lines
