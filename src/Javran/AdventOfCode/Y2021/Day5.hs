{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
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
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2021.Day5
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Bool
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (many)

data Day5

type Coord = (Int, Int)

type Line = (Coord, Coord)

coordP :: ReadP Coord
coordP = (,) <$> decimal1P <*> (char ',' *> decimal1P)

lineP :: ReadP Line
lineP = (,) <$> coordP <*> (string " -> " *> coordP)

allInThisRange x y = if x <= y then [x .. y] else [y .. x]

instance Solution Day5 where
  solutionIndex _ = (2021, 5)
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    pLines <- fmap (fromJust . consumeAllWithReadP lineP) . lines <$> getInputS
    let coverage :: M.Map Coord Int
        coverage = M.fromListWith (+) $ do
          ((a, b), (c, d)) <- pLines
          if
              | a == c -> [((a, i), 1) | i <- allInThisRange b d]
              | b == d -> [((i, b), 1) | i <- allInThisRange a c]
              | otherwise -> []
    answerShow $ countLength (>= 2) coverage
    let coverage2 :: M.Map Coord Int
        coverage2 = M.fromListWith (+) $ do
          ((a, b), (c, d)) <- pLines
          if
              | a == c -> [((a, i), 1) | i <- allInThisRange b d]
              | b == d -> [((i, b), 1) | i <- allInThisRange a c]
              | otherwise ->
                let dx = c - a
                    dy = d - b
                    l = abs dx
                    dx' = dx `div` l
                    dy' = dy `div` l
                 in if abs dx /= abs dy
                      then error "panic"
                      else fmap (,1) $ do
                        [(c, d)]
                          <> (takeWhile (/= (c, d)) $
                                iterate (\(u, v) -> (u + dx', v + dy')) (a, b))
    answerShow $ countLength (>= 2) coverage2
