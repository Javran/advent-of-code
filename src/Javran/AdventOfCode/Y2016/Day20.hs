{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
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

module Javran.AdventOfCode.Y2016.Day20
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Containers.ListUtils
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Ordered
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

data Day20 deriving (Generic)

type Range = (Int, Int)

rangeP :: ReadP Range
rangeP = (,) <$> (decimal1P <* char '-') <*> decimal1P

{-
  key: start index, value: end indices.
 -}
type BlockList = IM.IntMap IS.IntSet

isBlocked :: BlockList -> Int -> Bool
isBlocked bl addr = any isIn ls || any isIn c
  where
    isIn :: IS.IntSet -> Bool
    isIn xs = c' || not (IS.null r')
      where
        (_l', c', r') = IS.splitMember addr xs
    (ls, c, _) = IM.splitLookup addr bl

solve2 bl xs = case xs of
  [] -> 0
  [_] -> 1
  y : ys@(u : vs) ->
    if isBlocked bl (y + 1) then 1 + solve2 bl ys else u - y + 1 + solve2 bl vs

instance Solution Day20 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (consumeOrDie rangeP) . lines <$> getInputS
    let bl = IM.fromListWith (<>) do
          (l, r) <- xs
          pure (l, IS.singleton r)
        alts = IS.fromList do
          (l, r) <- xs
          v <- [l -1, r + 1]
          v <$ guard (v >= 0 && v <= 0xFFFF_FFFF)
        discreteBounds = filter (not . isBlocked bl) (IS.toAscList alts)
    answerShow $ head discreteBounds
    print (solve2 bl discreteBounds)
