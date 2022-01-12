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

module Javran.AdventOfCode.Y2017.Day6
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Lens
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
import Debug.Trace
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day6 deriving (Generic)

{-
  shift to left `offset` positions, do some length-preserving processing, and shift back.
 -}
withOffset n offset f xs = take n $ drop (n - offset) $ cycle ys
  where
    xs' = take n $ drop offset $ cycle xs
    ys = f xs'

{-
  Computes a balanced way to split `val` into `n` bins,
  allowing starting elements to be slightly larger.
 -}
balanced :: Int -> Int -> [Int]
balanced n val = fmap (\i -> if i <= r -1 then q + 1 else q) [0 .. n -1]
  where
    (q, r) = quotRem val n

redistribute :: Int -> [Int] -> [Int]
redistribute n xs = withOffset n (maxInd + 1) (zipWith (+) (balanced n maxVal)) (xs & ix maxInd .~ 0)
  where
    Just (Max (Arg maxVal maxInd)) = mconcat (zipWith (\v i -> Just (Max (Arg v i))) xs [0 :: Int ..])

findFix seen ~((i, x) : xs) =
  if S.member x seen
    then i
    else findFix (S.insert x seen) xs

instance Solution Day6 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (read @Int) . words . head . lines <$> getInputS
    let n = length xs
    print $ findFix mempty (zip [0 ..] $ iterate (redistribute n) xs)
