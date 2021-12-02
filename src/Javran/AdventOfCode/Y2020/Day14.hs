{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2020.Day14
  (
  )
where

{- HLINT ignore "Unused LANGUAGE pragma" -}

import Control.Monad
import Control.Monad.State.Strict
import Data.Bits
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import qualified Data.List.Split as LSplit
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP
import Data.Bifunctor

data Day14 -- TODO: change to actual number

-- TODO: import to Javran.AdventOfCode.Y2020.Main

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
maskToModifier xs = appEndo . foldMap (Endo . mk) $ zip [35,34..] xs
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

instance Solution Day14 where
  solutionIndex _ = (2020, 14)
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    instrs <- fmap (fromJust . consumeAllWithReadP instrP ) . lines <$> getInputS
    let (mem, _) = execState (mapM_ interpret instrs) (IM.empty, id)
    answerShow (sum mem)
