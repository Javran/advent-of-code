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

module Javran.AdventOfCode.Y2019.Day4
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

data Day4 deriving (Generic)

intToDigits :: Integral i => i -> [Int]
intToDigits x = ($ []) . foldr (.) id $ unfoldr f x
  where
    f 0 = Nothing
    f n = let (q, r) = n `quotRem` 10 in Just ((++ [fromIntegral r]), q)

solve :: Int -> Int -> [Int]
solve nFrom nTo = do
  n <- [nFrom .. nTo]
  [a, b, c, d, e, f] <- pure $ intToDigits n
  guard $ and $ zipWith (<=) [a, b, c, d, e] [b, c, d, e, f]
  guard $ or $ zipWith (==) [a, b, c, d, e] [b, c, d, e, f]
  pure n

solve2 :: Int -> Int -> [Int]
solve2 nFrom nTo = do
  n <- [nFrom .. nTo]
  [a, b, c, d, e, f] <- pure $ intToDigits n
  guard $ and $ zipWith (<=) [a, b, c, d, e] [b, c, d, e, f]
  guard $
    or $
      [ (a == b) && (b /= c)
      , (b == c) && (a /= b) && (c /= d)
      , (c == d) && (b /= c) && (d /= e)
      , (d == e) && (c /= d) && (e /= f)
      , (e == f) && (d /= e)
      ]
  pure n

instance Solution Day4 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    [nFrom, nTo] <- fmap (read @Int) . splitOn "-" . head . lines <$> getInputS
    answerShow $ length $ solve nFrom nTo
    answerShow $ length $ solve2 nFrom nTo
