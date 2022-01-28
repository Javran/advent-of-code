{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2015.Day2
  (
  )
where

import Data.Monoid
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day2 deriving (Generic)

type Box = (Int, Int, Int)

boxP :: ReadP Box
boxP =
  (,,)
    <$> decimal1P <* char 'x'
    <*> decimal1P <* char 'x'
    <*> decimal1P

paper, ribbon :: Box -> Int
paper (l, w, h) = minimum xs + 2 * sum xs
  where
    xs = [l * w, w * h, h * l]
ribbon (l, w, h) = 2 * minimum [l + w, w + h, h + l] + l * w * h

instance Solution Day2 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    boxes <- fmap (consumeOrDie boxP) . lines <$> getInputS
    let Just (Sum ans1, Sum ans2) =
          foldMap (\v -> Just (Sum $ paper v, Sum $ ribbon v)) boxes
    answerShow ans1
    answerShow ans2
