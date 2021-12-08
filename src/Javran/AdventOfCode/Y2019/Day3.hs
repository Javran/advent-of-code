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
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2019.Day3
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Bool
import Data.Char
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
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day3 deriving (Generic)

type Coord = (Int, Int)

data Dir = U | D | L | R deriving (Show)

type Instr = (Dir, Int)

instrP :: ReadP Instr
instrP = do
  d <-
    (U <$ char 'U')
      <++ (D <$ char 'D')
      <++ (L <$ char 'L')
      <++ (R <$ char 'R')
  n <- decimal1P
  pure (d, n)

applyDir :: Dir -> Coord -> Coord
applyDir = \case
  U -> first succ
  D -> first pred
  L -> second pred
  R -> second succ

applyInstr :: Instr -> State (Coord, S.Set Coord) ()
applyInstr (d, n) = replicateM_ n do
  (coord, acc) <- get
  let coord' = applyDir d coord
  put (coord', S.insert coord' acc)

instance Solution Day3 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    [xs, ys] <- fmap (consumeOrDie (instrP `sepBy` char ',')) . lines <$> getInputS
    let (_, setX) = execState (mapM_ applyInstr xs) ((0, 0), S.empty)
        (_, setY) = execState (mapM_ applyInstr ys) ((0, 0), S.empty)
    print $ minimum $ fmap (uncurry ((+) `on` abs)) $ S.toList (S.intersection setX setY)
