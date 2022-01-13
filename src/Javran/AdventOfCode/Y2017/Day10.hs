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

module Javran.AdventOfCode.Y2017.Day10
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
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
import Javran.AdventOfCode.TestExtra
import Javran.AdventOfCode.Y2017.Day6 (rotateLeftBy, rotateRightBy)
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day10 deriving (Generic)

{-
  A Circle (xs, offset) represents a list for the hash process,
  where:
  - the head of `xs` is always the current position
  - the list is rotated to left by `offset` positions,
    so that we can recover the underlying list by rotating to right the same amount.
 -}
type Circle = ([Int], Int)

step :: Int -> Int -> Int -> Circle -> Circle
step n len skipSize (xs0, offset) = (xs2, (offset + offset') `rem` n)
  where
    xs1 = let (ys, zs) = splitAt len xs0 in reverse ys <> zs
    offset' = len + skipSize
    xs2 = rotateLeftBy n (len + skipSize) xs1

instance Solution Day10 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (extraOps, rawInput) <- consumeExtra getInputS
    let xs = fmap (read @Int) . splitOn "," . head . lines $ rawInput
        n = case extraOps of
          Just raw -> read $ head raw
          Nothing -> 256
        initSt = ([0 .. n -1], 0)
        (fin, finOffset) = foldl' (\cir (len, skipSize) -> step n len skipSize cir) initSt $ zip xs [0 ..]
        fin' = rotateRightBy n finOffset fin
    do
      let x:y:_ = fin'
      answerShow $ x * y
