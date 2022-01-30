{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2016.Day15
  (
  )
where

import Javran.AdventOfCode.NumberTheory
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day15 deriving (Generic)

type Disc = (Int, Int, Int)

discP :: ReadP Disc
discP =
  (,,)
    <$> (string "Disc #" *> decimal1P)
    <*> (string " has " *> decimal1P)
    <*> (string " positions; at time=0, it is at position " *> decimal1P <* char '.')

solve :: [Disc] -> Integer
solve xs = case result of
  Just (SomeMod m) -> getVal m
  _ -> unreachable
  where
    result =
      chineseRemainder do
        (i, p, offset) <- xs
        pure $ fromIntegral (- i - offset) `modulo` fromIntegral p

instance Solution Day15 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (consumeOrDie discP) . lines <$> getInputS
    answerShow (solve xs)
    answerShow (solve (xs <> [(length xs + 1, 11, 0)]))
