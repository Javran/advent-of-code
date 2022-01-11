{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2017.Day1
  (
  )
where

import Data.Monoid
import Javran.AdventOfCode.Prelude

data Day1 deriving (Generic)

instance Solution Day1 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- head . lines <$> getInputS
    let check x y =
          if x == y
            then Sum (chToInt x)
            else 0
        n = length xs
        check2 i = check (xs !! i) (xs !! ((i + halve n) `rem` n))
    answerShow $
      getSum $ mconcat $ zipWith check xs (tail (cycle xs))
    answerShow $
      getSum $ mconcat $ fmap check2 [0 .. n -1]
