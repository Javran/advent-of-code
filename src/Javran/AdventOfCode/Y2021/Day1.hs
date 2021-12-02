{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2021.Day1
  (
  )
where

import Data.List
import Javran.AdventOfCode.Prelude

data Day1

instance Solution Day1 where
  solutionIndex _ = (2021, 1)
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (read @Int) . lines <$> getInputS
    answerShow $ length $ filter id $ zipWith (<) xs (tail xs)
    let xs' = fmap sum . filter ((== 3) . length) $ fmap (take 3) $ tails xs
    answerShow $ length $ filter id $ zipWith (<) xs' (tail xs')
