{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2019.Day1
  (
  )
where

import Javran.AdventOfCode.Prelude

data Day1 deriving (Generic)

instance Solution Day1 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (read @Int) . lines <$> getInputS
    let fuelCost = subtract 2 . floor @Double @Int . (/ 3) . fromIntegral
    answerShow $ sum $ fmap fuelCost xs
    let fuelCost2 = sum . takeWhile (> 0) . tail . iterate fuelCost
    answerShow $ sum $ fmap fuelCost2 xs
