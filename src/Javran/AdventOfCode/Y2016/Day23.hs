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
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2016.Day23
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Char
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day23 deriving (Generic)

data Reg = Reg Int deriving (Show)

mkReg :: Char -> Reg
mkReg ch = Reg $ ord ch - ord 'a'

-- immediate value or a value from register
type ReadVal = Either Int Reg

data InstrUn = Inc | Dec | Tgl deriving (Show)

data InstrBin = Jnz | Cpy deriving (Show)

data Instr
  = InstrUnary InstrUn ReadVal
  | InstrBinary InstrBin ReadVal ReadVal
  deriving (Show)

instrP :: ReadP Instr
instrP =
  foldl1'
    (<++)
    [ unary "inc" (InstrUnary Inc) readValP
    , unary "dec" (InstrUnary Dec) readValP
    , unary "tgl" (InstrUnary Tgl) readValP
    , binary "cpy" (InstrBinary Cpy) readValP readValP
    , binary "jnz" (InstrBinary Jnz) readValP readValP
    ]
  where
    sp = char ' '
    unary lit builder pa =
      string lit *> sp *> (builder <$> pa)
    binary lit builder pa pb =
      unary lit builder pa <*> (sp *> pb)

    intP = readS_to_P (reads @Int)
    regP = mkReg <$> satisfy (\ch -> ch >= 'a' && ch <= 'h')
    readValP = (Right <$> regP) <++ (Left <$> intP)

toggleInstr :: Instr -> Instr
toggleInstr = \case
  InstrUnary i x -> InstrUnary (tglUn i) x
  InstrBinary i x y -> InstrBinary (tglBin i) x y
  where
    tglUn = \case
      Inc -> Dec
      _ -> Inc
    tglBin = \case
      Jnz -> Cpy
      _ -> Jnz
interpret :: forall s. [Instr] -> ST s Int
interpret instrSrc = do
  regs <- VM.replicate 4 (0 :: Int)
  -- setup for login input.
  VM.unsafeWrite regs 0 7
  instrs <- V.unsafeThaw $ V.fromList instrSrc
  let getVal = \case
        Left i -> pure i
        Right (Reg i) -> VM.unsafeRead regs i
  fix
    do
      \go pc ->
        if pc < 0 || pc >= VM.length instrs
          then getVal $ Right (Reg 0)
          else
            VM.unsafeRead instrs pc >>= \case
              InstrUnary i ax -> do
                case i of
                  Inc
                    | Right (Reg x) <- ax ->
                      VM.unsafeModify regs (+ 1) x
                  Dec
                    | Right (Reg x) <- ax ->
                      VM.unsafeModify regs (subtract 1) x
                  Tgl -> do
                    offset <- getVal ax
                    let ind = pc + offset
                    when (ind >= 0 && ind < VM.length instrs) do
                      VM.unsafeModify instrs toggleInstr ind
                  _ -> pure ()
                go (pc + 1)
              InstrBinary i ax ay ->
                case i of
                  Cpy
                    | Right (Reg y) <- ay -> do
                      v <- getVal ax
                      VM.unsafeWrite regs y v
                      go (pc + 1)
                  Jnz -> do
                    cond <- getVal ax
                    offset <- getVal ay
                    go (pc + if cond /= 0 then offset else 1)
                  _ -> go (pc + 1)
    0

instance Solution Day23 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (consumeOrDie instrP) . lines <$> getInputS
    answerShow $ runST $ interpret xs
