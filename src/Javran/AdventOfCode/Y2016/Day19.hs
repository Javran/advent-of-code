{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2016.Day19
  (
  )
where

import Data.Bits
import Data.List
import Javran.AdventOfCode.Prelude

data Day19 deriving (Generic)

-- https://en.wikipedia.org/wiki/Josephus_problem
safePosition :: Int -> Int
safePosition n = 2 * l + 1
  where
    l = clearBit n (finiteBitSize @Int unreachable - countLeadingZeros n -1)

solve2 :: Int -> Int
solve2 n =
  if
      | x == n -> x
      | n < 2 * x -> n `rem` x
      | otherwise -> x + 2 * (n `rem` x)
  where
    pow3 = iterate (* 3) 1
    (_, x) : _ = dropWhile ((<= n) . fst) $ zip (tail pow3) pow3

{-
  My guess would be that this sort of problem is already studied
  somewhere, so that I can simulate it for some small numbers
  and probably we'll get a OEIS hit.

  And indeed we found it: http://oeis.org/A334473
 -}
_simulate :: Int -> [Int] -> [Int]
_simulate n xs =
  if n == 1
    then xs
    else _simulate (n -1) $ take (n -1) $ tail $ cycle ys
  where
    ys = delete (xs !! halve n) xs

instance Solution Day19 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    n <- read @Int . head . lines <$> getInputS
    answerShow (safePosition n)
    answerShow (solve2 n)
