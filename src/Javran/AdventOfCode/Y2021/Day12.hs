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

module Javran.AdventOfCode.Y2021.Day12
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Bool
import Data.Char
import Data.Either
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
import Debug.Trace
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, many)

data Day12 deriving (Generic)

isLarge :: String -> Bool
isLarge = any isUpper

search graph cur visitedSmalls acc
  | cur == "end" = pure (reverse $ cur : acc)
  | otherwise = do
    next <- S.toList (fromMaybe S.empty (graph M.!? cur))
    let large = isLarge next
    guard $ large || S.notMember next visitedSmalls
    search graph next (if not (isLarge cur) then S.insert cur visitedSmalls else visitedSmalls) (cur : acc)

search2 graph cur visitedSmalls twiceBudget acc
  | cur == "end" = pure (reverse $ cur : acc)
  | otherwise = do
    next <- S.toList (fromMaybe S.empty (graph M.!? cur))
    guard $ next /= "start"
    let large = isLarge next
        alreadyVisited = S.member next visitedSmalls
    guard $ large || (not alreadyVisited || twiceBudget)
    let visitedSmalls' = if not (isLarge cur) then S.insert cur visitedSmalls else visitedSmalls
    search2
      graph
      next
      visitedSmalls'
      (if large
         then twiceBudget
         else
           (if alreadyVisited then False else twiceBudget))
      (cur : acc)

instance Solution Day12 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap ((\[a, b] -> (a, b)) . splitOn "-") . lines <$> getInputS
    let graph = M.fromListWith (<>) do
          (u, v) <- xs
          [(u, S.singleton v), (v, S.singleton u)]
    answerShow $ length $ (search2 graph "start" (S.singleton "start") False [])
    answerShow $ length (search2 graph "start" (S.singleton "start") True [])
