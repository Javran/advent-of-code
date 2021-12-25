{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2021.Day7
  (
  )
where

import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Vector as V
import Javran.AdventOfCode.Prelude

data Day7 deriving (Generic)

instance Solution Day7 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- sort . fmap (read @Int) . splitOn "," . head . lines <$> getInputS
    let vs = V.fromList xs
        mid = vs V.! (V.length vs `quot` 2)
    answerShow $ sum $ fmap (\x -> abs (x - mid)) xs
    let cost x = let y = abs x in (y * (y + 1)) `quot` 2
        computeCost mz = sum (fmap (\x -> cost (x - mz)) xs)
        (lo, hi) = (V.head vs, V.last vs)
    answerShow $ minimum $ fmap computeCost [lo .. hi]
