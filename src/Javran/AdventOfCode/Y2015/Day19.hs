{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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

module Javran.AdventOfCode.Y2015.Day19
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Data.Char
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import qualified Data.List.Match as LMatch
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Monoid hiding (First, Last)
import qualified Data.PSQueue as PQ
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Debug.Trace
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day19 deriving (Generic)

type Str = BSC.ByteString

{-
  Splits the list at all possible positions.

  e.g.:
  > splitAts "abcde"
  [("","abcde"),("a","bcde"),("ab","cde"),("abc","de"),("abcd","e"),("abcde","")]

 -}
splitAts :: Str -> [] (Str, Str)
splitAts xs = do
  n <- [0 .. BSC.length xs]
  pure $ BSC.splitAt n xs

performReplace :: Rules -> Str -> [] Str
performReplace rules inp = do
  (pre, xs0) <- splitAts inp
  (lhs, rhss) <- rules
  Just xs1 <- pure $ BSC.stripPrefix lhs xs0
  rhs <- rhss
  pure $ pre <> rhs <> xs1

type Rules = [(Str, [Str])]

psearch rules steps q0 = case PQ.minView q0 of
  Nothing -> error "queue exhausted"
  Just (cur PQ.:-> Arg _l step, q1) ->
    if cur == "e"
      then step
      else
        let nexts = do
              next <- performReplace rules cur
              let step' = step + 1
              guard case HM.lookup next steps of
                Nothing -> True
                Just v -> v > step'
              pure (next, step')
            steps' = foldr (\(next, step') -> HM.insert next step') steps nexts
            q2 = foldr (\(next, step') -> PQ.insert next (Arg (BSC.length next) step')) q1 nexts
         in psearch rules steps' q2

chemP :: ReadP String
chemP = (:) <$> satisfy isAsciiUpper <*> munch isAsciiLower

{-
  TODO:
  Some analysis on my login input, which might or might not be useful:

  - only on rule's LHS: ["e"]

  - only on rule's RHS: ["Ar","C","Rn","Y"]

    + further, `Rn` and `Ar` always appear in pairs, in that order,
      and `Y` only appears between them.

 -}

instance Solution Day19 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    [rawRules, [inp]] <- splitOn [""] . lines <$> getInputS
    let inp' = BSC.pack inp
        rules = M.toAscList $ M.fromListWith (<>) $ fmap tr rawRules
          where
            tr x =
              let [a, b] = splitOn " => " x
               in (BSC.pack a, [BSC.pack b])
    answerShow (S.size $ S.fromList $ performReplace rules inp')
    do
      let lhs = S.fromList (fmap (BSC.unpack . fst) rules)
          rhs = S.fromList do
            (_, rhss) <- rules
            join $ fmap (consumeOrDie (many chemP) . BSC.unpack) rhss
      print $ lhs S.\\ rhs
      print $ rhs S.\\ lhs
