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

module Javran.AdventOfCode.Y2021.Day10
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
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
import Text.ParserCombinators.ReadP hiding (count, many)

data Day10 deriving (Generic)

findIllegal :: [] Char -> [] Char -> Maybe Char
findIllegal xs stk = case xs of
  [] -> Nothing
  y : ys -> case y of
    '(' -> findIllegal ys (')' : stk)
    '[' -> findIllegal ys (']' : stk)
    '{' -> findIllegal ys ('}' : stk)
    '<' -> findIllegal ys ('>' : stk)
    c -> case stk of
      [] -> Just c
      s : stk' ->
        if c == s
          then findIllegal ys stk'
          else Just c

findIllegal2 :: [] Char -> [] Char -> Maybe ([] Char)
findIllegal2 xs stk = case xs of
  [] -> Just stk
  y : ys -> case y of
    '(' -> findIllegal2 ys (')' : stk)
    '[' -> findIllegal2 ys (']' : stk)
    '{' -> findIllegal2 ys ('}' : stk)
    '<' -> findIllegal2 ys ('>' : stk)
    c -> case stk of
      [] -> Nothing
      s : stk' ->
        if c == s
          then findIllegal2 ys stk'
          else Nothing

instance Solution Day10 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- lines <$> getInputS
    do
      let ys = foldMap toScore $ mapMaybe (\v -> findIllegal v []) xs
          toScore = \case
            ')' -> 3 :: Sum Int
            ']' -> 57
            '}' -> 1197
            '>' -> 25137
            _ -> 0
      answerShow $ getSum ys
    do
      let ys = mapMaybe (\v -> findIllegal2 v []) xs
          toScore =
            foldl
              (\acc i ->
                 acc * 5
                   + (case i of
                        ')' -> 1 :: Int
                        ']' -> 2
                        '}' -> 3
                        '>' -> 4
                        _ -> 0))
              0
          zs = sort $ fmap toScore ys
          l = length zs
      answerShow (zs !! (l `quot` 2))
