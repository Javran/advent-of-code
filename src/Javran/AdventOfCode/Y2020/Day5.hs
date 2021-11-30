{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Javran.AdventOfCode.Y2020.Day5
  (
  )
where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Word
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP

data Day5

seatIdP :: ReadP Word16
seatIdP = do
  xs <- replicateM 7 fbP
  ys <- replicateM 3 lrP
  pure $ foldl (\acc i -> acc * 2 + i) 0 (xs <> ys)
  where
    fbP = (0 <$ char 'F') <++ (1 <$ char 'B')
    lrP = (0 <$ char 'L') <++ (1 <$ char 'R')

seatIdFromString :: String -> Word16
seatIdFromString = fromJust . consumeAllWithReadP seatIdP

findSeat :: [Word16] -> Word16
findSeat xs = case xs of
  a : b : _ | a + 2 == b -> a + 1
  _ : ys -> findSeat ys
  _ -> error "not found"

instance Solution Day5 where
  solutionIndex _ = (2020, 5)
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    rawLines <- lines <$> getInputS
    let xs = sort $ fmap seatIdFromString rawLines
    answerShow (last xs)
    answerShow (findSeat xs)
