{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2015.Day5
  (
  )
where

import Control.Monad
import Data.List
import Data.List.Split hiding (sepBy)
import Javran.AdventOfCode.Prelude

data Day5 deriving (Generic)

isNice :: String -> Bool
isNice xs = threeVowels && any (\(~[a, b]) -> a == b) vs && not (any (`elem` bans) vs)
  where
    threeVowels = case filter (`elem` "aeiou") xs of
      _1 : _2 : _3 : _ -> True
      _ -> False
    vs = divvy 2 1 xs
    bans = words "ab cd pq xy"

isNice2 :: String -> Bool
isNice2 xs = not (null cond1) && not (null cond2)
  where
    cond1 = do
      ys <- tails xs
      (us@[_, _], vs) <- pure $ splitAt 2 ys
      guard $ us `isInfixOf` vs
    cond2 = do
      [a, _, c] <- divvy 3 1 xs
      guard $ a == c

instance Solution Day5 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- lines <$> getInputS
    answerShow $ countLength isNice xs
    answerShow $ countLength isNice2 xs
