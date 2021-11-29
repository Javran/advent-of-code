{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Javran.AdventOfCode.Y2020.Day3
  ( main
  )
where

import Data.List
import qualified Data.Vector as V
import Javran.AdventOfCode.Prelude
import Control.Monad

data TreeMap = TreeMap
  { tmRows :: Int
  , tmCols :: Int
  , tmPayload :: V.Vector (V.Vector Bool)
  }
  deriving (Show)

type Coord = (Int, Int)
type Slope = (Int, Int)

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
treeMapIndex TreeMap {tmRows, tmCols, tmPayload} (r,cPre) = do
  guard $ r >= 0 && r < tmRows
  let c = cPre `mod` tmCols
  pure $ tmPayload V.! r V.! c

main :: IO ()
main = do
  tm <- mkTreeMap . lines <$> getInput 2020 3
  let slope = (1,3) :: Slope
      ts = unfoldr go (0, 0)
        where
          (dr, dc) = slope
          go coord@(r, c) = do
            t <- treeMapIndex tm coord
            pure (t, (r+dr, c+dc))
  print (length $ filter id ts)
