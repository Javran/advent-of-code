{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2017.Day2
  (
  )
where

import Control.Monad
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.TestExtra

data Day2 deriving (Generic)

result2 :: [Int] -> [Int]
result2 xs = do
  (x, xs0) <- pickInOrder xs
  y <- xs0
  (q, 0) <- [x `quotRem` y, y `quotRem` x]
  pure q

instance Solution Day2 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (extraOps, rawInput) <- consumeExtra getInputS
    let xs = fmap (fmap (read @Int) . words) . lines $ rawInput
        (runPart1, runPart2) = shouldRun extraOps
    when runPart1 do
      answerShow $
        sum $
          fmap
            (\row -> let Just (MinMax (u, v)) = foldMap (Just . minMax) row in v - u)
            xs
    when runPart2 do
      answerShow $ sum $ fmap (head . result2) xs
