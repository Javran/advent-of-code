{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2017.Day13
  (
  )
where

import Data.Monoid
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day13 deriving (Generic)

type Scanner = (Int, Int)

scannerP :: ReadP Scanner
scannerP = (,) <$> (decimal1P <* string ": ") <*> decimal1P

caught :: Int -> Scanner -> Bool
caught t0 (dep, rg) =
  {-
    Despite that the scanner moves back and forth,
    we only care about whether the scanner is at top
    at the start of time when our packet reaches there.
    This means this behavior can still be treated as if it's a simple
    linear function modulo period.
   -}
  (dep + t0) `rem` ((rg -1) * 2) == 0

severity :: Int -> Scanner -> Sum Int
severity t0 sc@(dep, rg) = if caught t0 sc then Sum (dep * rg) else 0

instance Solution Day13 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (consumeOrDie scannerP) . lines <$> getInputS
    do
      let Sum ans = foldMap (severity 0) xs
      answerShow ans
    do
      {-
        TODO: I'm wondering if Chinese remainder theorem is applicable here?
       -}
      answerShow $ head $ filter (\t0 -> not (any (caught t0) xs)) [1 ..]
