{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2020.Day9
  (
  )
where

import Control.Monad
import Data.List
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Vector as V
import Javran.AdventOfCode.Prelude

data Day9

-- TODO: example needs 5 and actual input needs 25,
-- there is no way to vary this value for example and real run for now.
preambleLength :: Int
preambleLength = 25

instance Solution Day9 where
  solutionIndex _ = (2020, 9)
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (read @Integer) . lines <$> getInputS
    let targetSum = head $ do
          ns <- tails xs
          (prevs, cur : _) <- pure (splitAt preambleLength ns)
          let prevSet = S.fromList prevs
          guard $ all (\x -> let y = cur - x in S.notMember y prevSet) (S.toList prevSet)
          pure cur
    answerShow targetSum
    do
      let ys = takeWhile (/= targetSum) xs
          ysAcc = V.fromList $ scanl (+) 0 ys
          len = length ys
          computeSum i j =
            -- just an efficient way to compute `sum (fmap (ys !!) [i..j])`
            (ysAcc V.! (j + 1)) - (ysAcc V.! i)
      answerShow $
        head $ do
          lInd <- [0 .. len -1]
          let mayR = binarySearch (lInd + 1) (len -1)
              binarySearch loInd hiInd = do
                guard $ loInd <= hiInd
                let midInd = (loInd + hiInd) `quot` 2
                case computeSum lInd midInd `compare` targetSum of
                  LT -> binarySearch (midInd + 1) hiInd
                  EQ -> Just midInd
                  GT -> binarySearch loInd (midInd -1)
          Just rInd <- [mayR]
          let sliced = fmap (\i -> computeSum i i) [lInd .. rInd]
              Just (Max a, Min b) =
                foldMap (\i -> Just (Max i, Min i)) sliced
          pure $ a + b
