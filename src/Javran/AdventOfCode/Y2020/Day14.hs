{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Javran.AdventOfCode.Y2020.Day14
  (
  )
where

import Control.Monad
import Control.Monad.State.Strict
import Data.Bits
import qualified Data.IntMap.Strict as IM
import Data.Monoid
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP

data Day14 deriving (Generic)

data MaskElem = MX | M0 | M1

type Mask = [MaskElem]

data Instr
  = InstrSetMask Mask -- mask = ...
  | InstrAssign Int Int -- mem[x] = y

type MachineState = (IM.IntMap Int, Int -> Int)

instrP :: ReadP Instr
instrP = setMaskP <++ assignP
  where
    setMaskP = do
      _ <- string "mask = "
      let bitOpP =
            (MX <$ char 'X')
              <++ (M1 <$ char '1')
              <++ (M0 <$ char '0')
      InstrSetMask <$> replicateM 36 bitOpP
    assignP = do
      _ <- string "mem["
      x <- decimal1P
      _ <- string "] = "
      y <- decimal1P
      pure $ InstrAssign x y

maskToModifier :: Mask -> Int -> Int
maskToModifier xs = appEndo . foldMap (Endo . mk) $ zip [35, 34 ..] xs
  where
    mk :: (Int, MaskElem) -> Int -> Int
    mk (i, m) = case m of
      MX -> id
      M1 -> (`setBit` i)
      M0 -> (`clearBit` i)

interpret :: Instr -> State MachineState ()
interpret instr = case instr of
  InstrSetMask m -> modify (second (const (maskToModifier m)))
  InstrAssign addr valPre -> do
    modifier <- gets snd
    let val = modifier valPre
    modify (first (IM.insert addr val))

maskToAddrModifier :: Mask -> Int -> [Int]
maskToAddrModifier xs addr = fmap pack (sequence nondetAddr)
  where
    pack ds = foldl (\acc i -> acc * 2 + if i then 1 else 0) 0 ds
    nondetAddr :: [] [Bool]
    nondetAddr = zipWith (\loc m -> maskElemToModifier m (testBit addr loc)) [35, 34 ..] xs
    maskElemToModifier :: MaskElem -> Bool -> [Bool]
    maskElemToModifier m v = case m of
      M0 -> [v]
      M1 -> [True]
      MX -> [False, True]

type MachineState2 = (IM.IntMap Int, Int -> [Int])

interpret2 :: Instr -> State MachineState2 ()
interpret2 instr = case instr of
  InstrSetMask m -> modify (second (const (maskToAddrModifier m)))
  InstrAssign addrPre val -> do
    modifier <- gets snd
    let addrs = modifier addrPre
        newM = IM.fromList $ do
          addr <- addrs
          pure (addr, val)
    modify (first (IM.union newM))

instance Solution Day14 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    instrs <- fmap (fromJust . consumeAllWithReadP instrP) . lines <$> getInputS
    do
      let (mem, _) = execState (mapM_ interpret instrs) (IM.empty, id)
      answerShow (sum mem)
    do
      let (mem, _) = execState (mapM_ interpret2 instrs) (IM.empty, pure)
      answerShow (sum mem)
