{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2021.Day9
  (
  )
where

import Control.Monad
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Char
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord
import qualified Data.Set as S
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude

data Day9 deriving (Generic)

chToInt :: Char -> Int
chToInt ch = ord ch - ord '0'

type Coord = (Int, Int)

neighbors :: Coord -> [] Coord
neighbors c =
  fmap
    ($ c)
    [ first succ
    , first pred
    , second succ
    , second pred
    ]

findBasin :: M.Map Coord Int -> Coord -> State (S.Set Coord) ()
findBasin heights coord = do
  let h :: Int
      h = heights M.! coord
  visited <- get
  unless (S.member coord visited || h == 9) do
    modify (S.insert coord)
    let coords :: [Coord]
        coords = filter (`M.member` heights) (neighbors coord)
    forM_ coords $ \coord' -> do
      let h' = heights M.! coord'
      when (S.notMember coord' visited && h' > h) $
        findBasin heights coord'

instance Solution Day9 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (fmap chToInt) . lines <$> getInputS
    let heights = M.fromList do
          (r, row) <- zip [0 ..] xs
          (c, x) <- zip [0 ..] row
          pure ((r, c), x)
        lowPoints = do
          (coord, h) <- M.toList heights
          let surroundingHs = mapMaybe (\c' -> heights M.!? c') (neighbors coord)
          guard $ all (> h) surroundingHs
          pure (coord, h)
    answerShow $ sum $ fmap (succ . snd) lowPoints
    answerShow $
      product $
        take 3 $
          sortOn Down do
            (c, _) <- lowPoints
            pure $ S.size $ execState (findBasin heights c) S.empty
