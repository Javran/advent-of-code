{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2018.Day16
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Lens hiding (universe)
import Control.Monad
import Data.Bits
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
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day16 deriving (Generic)

data Register = R0 | R1 | R2 | R3 deriving (Enum)

data ValueMode = Reg | Imm deriving (Enum, Bounded, Show)

{-
  TODO: How to better model this, if we were to generalize to "higher dimensions",
  that say, only few constructs are forbidden?
 -}
data BinValueMode
  = ImmReg
  | RegImm
  | RegReg
  deriving (Enum, Bounded, Show)

data OpType
  = Add ValueMode
  | Mul ValueMode
  | BitAnd ValueMode
  | BitOr ValueMode
  | Assign ValueMode
  | TestGreaterThan BinValueMode
  | TestEqual BinValueMode
  deriving (Show)

type DeviceState = (Int, Int, Int, Int)

allOpTypes :: [OpType]
allOpTypes =
  ([Add, Mul, BitAnd, BitOr, Assign] <*> universe)
    <> ([TestGreaterThan, TestEqual] <*> universe)

interpret :: DeviceState -> OpType -> (Int, Int, Int) -> Maybe DeviceState
interpret ds opType (a, b, c) = case opType of
  Add mb -> cat0 mb (+)
  Mul mb -> cat0 mb (*)
  BitAnd mb -> cat0 mb (.&.)
  BitOr mb -> cat0 mb (.|.)
  Assign ma -> do
    v0 <- getVal ma a
    rOut <- resolveReg c
    pure $ ds & _r rOut .~ v0
  TestGreaterThan mab -> cat1 mab (>)
  TestEqual mab -> cat1 mab (==)
  where
    -- category0 covers ops with prefix add / mul / ban / bor
    cat0 mb f = do
      v0 <- getVal Reg a
      v1 <- getVal mb b
      rOut <- resolveReg c
      pure $ ds & _r rOut .~ (f v0 v1)
    -- category1 covers ops with prefix gt / eq
    cat1 mab f = do
      let (ma, mb) = case mab of
            ImmReg -> (Imm, Reg)
            RegImm -> (Reg, Imm)
            RegReg -> (Reg, Reg)
      v0 <- getVal ma a
      v1 <- getVal mb b
      rOut <- resolveReg c
      pure $ ds & _r rOut .~ (bool 0 1 (f v0 v1))
    _r = \case
      R0 -> _1
      R1 -> _2
      R2 -> _3
      R3 -> _4

    getVal vm i = case vm of
      Reg -> do
        r <- resolveReg i
        pure $ ds ^. _r r
      Imm -> pure i

    resolveReg :: Int -> Maybe Register
    resolveReg i =
      toEnum @Register i <$ guard (i >= 0 && i <= 3)

instance Solution Day16 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap id . lines <$> getInputS
    forM_ allOpTypes $ \opTyp -> do
      print opTyp
      print (interpret (3, 2, 1, 1) opTyp (2, 1, 2))
