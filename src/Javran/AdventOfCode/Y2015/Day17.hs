{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2015.Day17
  (
  )
where

import Data.Function.Memoize (memoFix)
import qualified Data.Vector as V
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.TestExtra

data Day17 deriving (Generic)

{-
  Dynamic programming problem, ref:

  https://en.wikipedia.org/wiki/Knapsack_problem#0-1_knapsack_problem

  Here I used memoization rather than any Array-based solution,
  as memoization is simple to implement (though could be a bit slower).

 -}
instance Solution Day17 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (extras, rawInput) <- consumeExtra getInputS
    let xs = V.fromList . fmap (read @Int) . lines $ rawInput
        n = V.length xs
        target = singleLineExtra 150 extras
    do
      let dp :: (Int, Int) -> Int
          dp = memoFix \q (w, i) ->
            if i == -1
              then (if w == 0 then 1 else 0)
              else q (w, i -1) + q (w - xs V.! i, i -1)
      answerShow $ dp (target, n -1)
    do
      let dp :: (Int, Int, Int) -> Int
          dp = memoFix \q (w, i, cnt) ->
            if i == -1
              then (if w == 0 && cnt == 0 then 1 else 0)
              else q (w, i -1, cnt) + q (w - xs V.! i, i -1, cnt -1)
      answerShow $
        head $
          dropWhile (== 0) $
            fmap (\i -> dp (target, n -1, i)) [1 .. n]
