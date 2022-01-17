{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Javran.AdventOfCode.Y2016.Day6
  (
  )
where

import Data.List
import qualified Data.Map.Strict as M
import Data.Semigroup
import Javran.AdventOfCode.Prelude

data Day6 deriving (Generic)

leastAndMostFrequent :: String -> (Char, Char)
leastAndMostFrequent xs = (chMin, chMax)
  where
    Just (MinMax (Arg _ chMin, Arg _ chMax)) = foldMap Just freqs
    freqs =
      fmap (\(ch, cnt) -> minMax (Arg cnt ch))
        . M.toList
        . M.fromListWith (+)
        $ fmap (,1 :: Int) xs

instance Solution Day6 where
  solutionRun _ SolutionContext {getInputS, answerS} = do
    xs <- transpose . lines <$> getInputS
    let results = fmap leastAndMostFrequent xs
    answerS $ fmap snd results
    answerS $ fmap fst results
