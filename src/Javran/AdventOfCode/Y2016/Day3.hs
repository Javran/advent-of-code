{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2016.Day3
  (
  )
where

import Data.List
import Data.List.Split hiding (sepBy)
import Javran.AdventOfCode.Prelude

data Day3 deriving (Generic)

type Tup3 = (Int, Int, Int)

{-
  Sorting network for 3 elements

  TODO: we can probably try this out: https://doisinkidney.com/posts/2018-05-06-sorting-small.html
 -}
sortSides :: Tup3 -> Tup3
sortSides (a0, b0, c0) = (a2, b2, c3)
  where
    MinMax (a1, b1) = minMaxFromPair (a0, b0)
    MinMax (b2, c2) = minMaxFromPair (b1, c0)
    MinMax (a2, c3) = minMaxFromPair (a1, c2)

isTriangle :: Tup3 -> Bool
isTriangle = (\(a, b, c) -> a + b > c) . sortSides

countTriangles :: [[Int]] -> Int
countTriangles = countLength isTriangle . fmap (\(~[a, b, c]) -> (a, b, c))

instance Solution Day3 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (fmap (read @Int) . words) . lines <$> getInputS
    answerShow $ countTriangles xs
    answerShow $ countTriangles (concatMap (chunksOf 3) $ transpose xs)
