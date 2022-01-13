{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2017.Day6
  ( rotateLeftBy
  , rotateRightBy
  )
where

import Control.Lens
import qualified Data.Map.Strict as M
import Data.Semigroup
import Javran.AdventOfCode.Prelude

data Day6 deriving (Generic)

{-
  Rotates a known-length list to left or right.
  n > 0, 0 <= offset <= n.

  TODO: QuickCheck, make this accept nums in other ranges.

 -}
rotateLeftBy, rotateRightBy :: Int -> Int -> [a] -> [a]
rotateLeftBy n offset xs = take n $ drop offset $ cycle xs
rotateRightBy n offset = rotateLeftBy n (n - offset)

{-
  Computes a balanced way to split `val` into `n` bins,
  allowing starting elements to be slightly larger.
 -}
balanced :: Int -> Int -> [Int]
balanced n val = fmap (\i -> if i <= r -1 then q + 1 else q) [0 .. n -1]
  where
    (q, r) = quotRem val n

redistribute :: Int -> [Int] -> [Int]
redistribute n xs =
  zipWith
    (+)
    (xs & ix maxInd .~ 0)
    (rotateRightBy n (maxInd + 1) $ balanced n maxVal)
  where
    {-
      This relies on the fact that `Ord` instance of `Arg` is left-biased,
      so when the value ties, the leftmost one wins.
     -}
    Just (Max (Arg maxVal maxInd)) =
      mconcat (zipWith (\v i -> Just (Max (Arg v i))) xs [0 :: Int ..])

findFix :: Ord a => M.Map a Int -> [(Int, a)] -> (Int, Int)
findFix seen ~((j, x) : xs) = case seen M.!? x of
  Just i ->
    {- return value indicates that current value `x`, tagged `j`,
       has been seen before with another tag `i`.
     -}
    (i, j)
  Nothing -> findFix (M.insert x j seen) xs

instance Solution Day6 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (read @Int) . words . head . lines <$> getInputS
    let n = length xs
        (i, j) = findFix mempty (zip [0 ..] $ iterate (redistribute n) xs)
    answerShow j
    answerShow (j - i)
