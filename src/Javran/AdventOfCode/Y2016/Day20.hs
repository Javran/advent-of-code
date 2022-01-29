{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2016.Day20
  (
  )
where

import Control.Monad
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.TestExtra
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day20 deriving (Generic)

type Range = (Int, Int)

rangeP :: ReadP Range
rangeP = (,) <$> (decimal1P <* char '-') <*> decimal1P

{-
  key: start index, value: end indices.
 -}
type BlockList = IM.IntMap IS.IntSet

isBlocked :: BlockList -> Int -> Bool
isBlocked bl addr = any isIn ls || any isIn c
  where
    isIn :: IS.IntSet -> Bool
    isIn xs = c' || not (IS.null r')
      where
        (_l', c', r') = IS.splitMember addr xs
    (ls, c, _) = IM.splitLookup addr bl

{-
  This function assumes that:

  - xs is sorted and contain only numbers in range,
    with the exception of the last one, which is maxRange + 1

  - two assumptions that are not required but makes it easier to think about:

    + assume that 0 is blocked
    + assume that maxRange + 1 is not blocked

 -}
solve2 :: BlockList -> [Int] -> Int
solve2 bl xs = case xs of
  [] -> 0
  [_] -> 1
  y : ys@(z : zs) ->
    -- test y's right side
    if isBlocked bl (y + 1)
      then -- y+1 is blocked, meaning this discrete bound stands on its own.
        1 + solve2 bl ys
      else -- now we can establish a consecutive "allowed zone" by taking next value
        z - y + 1 + solve2 bl zs

instance Solution Day20 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (extraOps, rawInput) <- consumeExtra getInputS
    let xs = fmap (consumeOrDie rangeP) . lines $ rawInput
        maxRange' :: Integer
        maxRange' = singleLineExtra 0xFFFF_FFFF extraOps
        bl = IM.fromListWith (<>) do
          (l, r) <- xs
          pure (l, IS.singleton r)
        maxRange :: Int
        maxRange = either (\msg -> error $ "requirement unmet: " <> msg) id do
          when (fromIntegral @_ @Integer (maxBound @Int) < maxRange') do
            Left "size of Int is too small"
          unless (isBlocked bl 0) do
            Left "assumed 0 is blocked, which is not the case."
          let r = fromInteger maxRange'
          when (isBlocked bl (r + 1)) do
            Left "assumed max value + 1 is not blocked, which is not the case."
          pure r
        alts = IS.fromList do
          (l, r) <- xs
          {-
            to detect consecutive allowed ranges, we can just probe
            immediate left and immediate right of all listed ranges.
           -}
          v <- [l -1, r + 1]
          v <$ guard (v >= 0 && v <= maxRange)
        discreteBounds = filter (not . isBlocked bl) (IS.toAscList alts)
    answerShow $ head discreteBounds
    answerShow $ solve2 bl (discreteBounds <> [maxRange + 1]) - 1
