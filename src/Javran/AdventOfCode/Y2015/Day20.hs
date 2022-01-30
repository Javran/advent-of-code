{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2015.Day20
  (
  )
where

import Control.Monad
import Javran.AdventOfCode.NumberTheory (sumOfDivisors)
import Javran.AdventOfCode.Prelude
import Math.NumberTheory.ArithmeticFunctions

data Day20 deriving (Generic)

presentCount :: Int -> Int
presentCount = (* 10) . sumOfDivisors

presentCount2 :: Int -> Int
presentCount2 n = sum do
  t <- divisorsList n
  guard $ n <= 50 * t
  pure (t * 11)

instance Solution Day20 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    n <- read . head . lines <$> getInputS
    do
      let ys = fmap (\i -> (i, presentCount i)) [1 ..]
          (ans1, _) : _ = dropWhile ((< n) . snd) ys
      answerShow ans1
    do
      let ys = fmap (\i -> (i, presentCount2 i)) [1 ..]
          (ans2, _) : _ = dropWhile ((< n) . snd) ys
      answerShow ans2
