{-# LANGUAGE DeriveGeneric #-}
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
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude

data Day9 deriving (Generic)

instance Solution Day9 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (mExtra, xsPre) <- consumeExtraLeadingLines <$> getInputS
    let preambleLength = maybe 25 (read . unlines) mExtra
        xs = fmap (read @Integer) . lines $ xsPre
        targetSum = head $ do
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
