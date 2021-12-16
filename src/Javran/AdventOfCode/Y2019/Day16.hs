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

module Javran.AdventOfCode.Y2019.Day16
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
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, many)

data Day16 deriving (Generic)

genSeq :: Int -> [Int]
genSeq n = tail (cycle basePat)
  where
    basePat = concatMap (replicate n) [0, 1, 0, -1]

lastDigit :: Int -> Int
lastDigit n = case compare n 0 of
  GT -> n `rem` 10
  EQ -> 0
  LT -> - (n `mod` (-10))

onePhase :: Int -> [Int] -> [Int]
onePhase len xs = fmap (\i -> lastDigit $ sum $ zipWith (*) xs (genSeq i)) [1 .. len]

sndHalfOnePhaseRev :: [Int] -> [Int]
sndHalfOnePhaseRev = tail . scanl (\x y -> (x + y) `rem` 10) 0

instance Solution Day16 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (extraOps, rawInput) <- consumeExtraLeadingLines <$> getInputS
    let xs = fmap chToInt . head . lines $ rawInput
        len = length xs
        runPart1 = maybe True ("part1" `elem`) extraOps
        runPart2 = maybe True ("part2" `elem`) extraOps
    when runPart1 do
      answerShow $ digitsToInt @Int (take 8 $ iterate (onePhase len) xs !! 100)
    when runPart2 do
      let offset = digitsToInt (take 7 xs)
          cutRev = take cutLen (cycle (reverse xs))
          cutLen = len * 10000 - offset
          ans = drop (cutLen - 8) $ iterate sndHalfOnePhaseRev cutRev !! 100
      -- TODO: still slow.
      answerShow $ digitsToInt @Int (reverse ans)
