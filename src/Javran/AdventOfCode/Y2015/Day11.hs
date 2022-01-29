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

module Javran.AdventOfCode.Y2015.Day11
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
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day11 deriving (Generic)

{-
  The representation is reversed so that we can increase from
  the least significant end.
 -}
type RevPass = String

bans :: [] Char
bans = "iol"

incr :: RevPass -> RevPass
incr = incrStd

incrStd :: RevPass -> RevPass
incrStd = \case
  [] -> []
  c : ch ->
    if
        | c == 'z' -> 'a' : incrStd ch
        | c `elem` preBans ->
          let c' = chr (ord c + 2)
           in if not checked || c' <= 'z'
                then c' : ch
                else error "resulting value out of range"
        | otherwise -> chr (ord c + 1) : ch
  where
    checked = False
    preBans = fmap pred bans

isCompliant :: RevPass -> Bool
isCompliant xs = not (null twoPairs) && not (null incrStraight) && not (any (`elem` bans) xs)
  where
    incrStraight = do
      ~[c, b, a] <- divvy 3 1 xs
      guard $ [a, b, c] == take 3 [a ..]
    twoPairs = do
      ts <- tails xs
      ([a, b], ys) <- pure $ splitAt 2 ts
      guard $ a == b
      guard $ any (\[c, d] -> c == d) $ divvy 2 1 ys

instance Solution Day11 where
  solutionRun _ SolutionContext {getInputS, answerS} = do
    xs <- reverse . head . lines <$> getInputS
    let gen = tail $ iterate incr xs
        ans1 : ans2 : _ = filter isCompliant $ gen
    answerS $ reverse ans1
    answerS $ reverse ans2
