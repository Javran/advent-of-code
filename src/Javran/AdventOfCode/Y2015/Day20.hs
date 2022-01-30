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

module Javran.AdventOfCode.Y2015.Day20
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
import Data.Monoid hiding (First, Last)
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Math.NumberTheory.Primes
import Text.ParserCombinators.ReadP hiding (count, get, many)
import Math.NumberTheory.ArithmeticFunctions

data Day20 deriving (Generic)

sumOfProperDivisors :: Int -> Int
sumOfProperDivisors = product . fmap f . factorise
  where
    f (p, m) = sum (take (1 + fromIntegral m) $ iterate (* unPrime p) 1)

solve2 :: Int -> Int
solve2 n = sum do
   t <- ts
   guard $ n `quot` t <= 50
   pure (t * 11)
  where
    ts = divisorsList n

instance Solution Day20 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    n <- read @Int . head . lines <$> getInputS
    do
      let ys = fmap (\v -> (v, (* 10) . sumOfProperDivisors $ v)) [1 ..]
          (ans1, _) : _ = dropWhile ((< n) . snd) ys
      answerShow ans1
    do
      let ys = fmap (\i -> (i, solve2 i)) [1..]
          (ans2, _) : _ = dropWhile ((< n) . snd) ys
      answerShow ans2
