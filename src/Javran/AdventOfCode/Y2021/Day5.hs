{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Javran.AdventOfCode.Y2021.Day5
  (
  )
where

import Data.Bifunctor
import qualified Data.Map.Strict as M
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (many)

data Day5 deriving (Generic)

type Coord = (Int, Int)

type Line = (Coord, Coord)

coordP :: ReadP Coord
coordP = (,) <$> decimal1P <*> (char ',' *> decimal1P)

lineP :: ReadP Line
lineP = (,) <$> coordP <*> (string " -> " *> coordP)

getAllInRange :: (Ord a, Enum a) => a -> a -> [a]
getAllInRange x y = if x <= y then [x .. y] else [y .. x]

instance Solution Day5 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    pLines <- fmap (fromJust . consumeAllWithReadP lineP) . lines <$> getInputS
    let coverage :: (Line -> [(Coord, Int)]) -> M.Map Coord Int
        coverage handleDiagonal = M.fromListWith (+) do
          l@((a, b), (c, d)) <- pLines
          if
              | a == c -> [((a, i), 1) | i <- getAllInRange b d]
              | b == d -> [((i, b), 1) | i <- getAllInRange a c]
              | otherwise -> handleDiagonal l
        ignoreDiagonal = const []
    answerShow $ countLength (>= 2) (coverage ignoreDiagonal)
    let countDiagonal (begin@(a, b), (c, d)) =
          if abs dca /= abs ddb
            then error "panic"
            else fmap (,1) . take (l + 1) $ iterate (bimap (+ dx) (+ dy)) begin
          where
            (dca, ddb) = (c - a, d - b)
            l = abs dca
            (dx, dy) = (dca `div` l, ddb `div` l)
    answerShow $ countLength (>= 2) (coverage countDiagonal)
