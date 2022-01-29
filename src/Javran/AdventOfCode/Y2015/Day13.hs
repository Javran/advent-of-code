{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2015.Day13
  (
  )
where

import Data.Char
import Data.Containers.ListUtils (nubOrd)
import Data.List
import qualified Data.List.Match as LMatch
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day13 deriving (Generic)

type Name = String

type Happiness = ((Name, Name), Int)

happinessP :: ReadP Happiness
happinessP = do
  let wP = munch1 isLetter
  a <- wP
  strP " would "
  f <-
    (id <$ strP "gain ")
      <++ (negate <$ strP "lose ")
  v <- decimal1P
  strP " happiness units by sitting next to "
  b <- wP
  charP '.'
  pure ((a, b), f v)

type HappinessInfo = M.Map (Name, Name) Int

happiness :: HappinessInfo -> [Name] -> Int
happiness hInfo xs =
  sum . fmap compute . LMatch.take xs $ divvy 3 1 (cycle xs)
  where
    compute ~[a, b, c] =
      fromMaybe 0 (hInfo M.!? (b, a))
        + fromMaybe 0 (hInfo M.!? (b, c))

solve :: HappinessInfo -> [Name] -> Int
solve hInfo (~(hd : tl)) = maximum . fmap (happiness hInfo) $ perms
  where
    {-
      Since we are looking at a circular list, it doesn't matter which
      element is the first one, so we can fix an arbitrary one
      to reduce search space.
     -}
    perms = fmap (hd :) $ permutations tl

instance Solution Day13 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (consumeOrDie happinessP) . lines <$> getInputS
    let hInfo = M.fromList xs
        names = nubOrd do
          ((a, b), _) <- xs
          [a, b]
    answerShow $ solve hInfo names
    answerShow $ solve hInfo $ "" : names
