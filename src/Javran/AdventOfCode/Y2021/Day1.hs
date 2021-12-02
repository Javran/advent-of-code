{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2021.Day1
  (
  )
where

import Data.List.Split
import Javran.AdventOfCode.Prelude

data Day1

countIncr :: [Int] -> Int
countIncr xs = countLength id $ zipWith (<) xs (tail xs)

instance Solution Day1 where
  solutionIndex _ = (2021, 1)
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (read @Int) . lines <$> getInputS
    answerShow $ countIncr xs
    let xs' = fmap sum $ divvy 3 1 xs
    answerShow $ countIncr xs'
