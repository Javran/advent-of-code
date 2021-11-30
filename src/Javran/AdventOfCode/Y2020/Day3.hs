{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Javran.AdventOfCode.Y2020.Day3
  (
  )
where

import Control.Monad
import Data.List
import Data.Monoid
import qualified Data.Vector as V
import Javran.AdventOfCode.Prelude

data Day3

instance Solution Day3 where
  solutionIndex _ = (2020, 3)
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    tm <- mkTreeMap . lines <$> getInputS
    answerShow $ getSum (countTrees tm (1, 3))
    answerShow $
      product $ do
        getSum . countTrees tm <$> [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]

data TreeMap = TreeMap
  { tmRows :: Int
  , tmCols :: Int
  , tmPayload :: V.Vector (V.Vector Bool)
  }
  deriving (Show)

type Coord = (Int, Int)

type Slope = (Int, Int) -- note that the order is row then col for consistency.

mkTreeMap :: [] String -> TreeMap
mkTreeMap xs =
  TreeMap
    { tmRows
    , tmCols
    , tmPayload = V.fromList $ fmap (V.fromListN tmCols . fmap tr) xs
    }
  where
    tr '#' = True
    tr '.' = False
    tr ch = error $ "Unknown input: " <> show ch
    tmRows = length xs
    tmCols = length $ head xs

treeMapIndex :: TreeMap -> Coord -> Maybe Bool
treeMapIndex TreeMap {tmRows, tmCols, tmPayload} (r, cPre) = do
  guard $ r >= 0 && r < tmRows
  let c = cPre `mod` tmCols
  pure $ tmPayload V.! r V.! c

countTrees :: TreeMap -> Slope -> Sum Int
countTrees tm (dr, dc) = mconcat $ unfoldr go (0, 0)
  where
    go coord@(r, c) = do
      t <- treeMapIndex tm coord
      pure (if t then 1 else 0, (r + dr, c + dc))
