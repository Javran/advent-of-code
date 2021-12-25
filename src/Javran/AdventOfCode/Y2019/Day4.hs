{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2019.Day4
  (
  )
where

import Control.Monad
import Data.List.Split hiding (sepBy)
import Javran.AdventOfCode.Prelude

data Day4 deriving (Generic)

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
    or
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
