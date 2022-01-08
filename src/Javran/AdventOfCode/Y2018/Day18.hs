{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Javran.AdventOfCode.Y2018.Day18
  (
  )
where

import Control.Monad
import qualified Data.DList as DL
import Data.Foldable
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Javran.AdventOfCode.Prelude

data Day18 deriving (Generic)

type Coord = (Int, Int) -- row, col

type CoordSet = S.Set Coord

type World = (CoordSet, CoordSet) -- trees, lumberyard

type PreWorld = (DL.DList Coord, DL.DList Coord)

mkTree, mkYard :: Coord -> PreWorld
mkTree = (,mempty) . DL.singleton
mkYard = (mempty,) . DL.singleton

-- use with care - the input list must already be ordered.
packWorld :: PreWorld -> World
packWorld = bimap f f
  where
    f = S.fromDistinctAscList . toList

parseFromRaw :: [String] -> ((Int, Int), World)
parseFromRaw xs = ((rows, coys), packWorld (ts, ys))
  where
    rows = length xs
    coys = length (head xs)
    (ts, ys) = mconcat do
      (r, row) <- zip [0 ..] xs
      (c, ch) <- zip [0 ..] row
      pure case ch of
        '|' -> mkTree (r, c)
        '#' -> mkYard (r, c)
        _ -> mempty

adjacents :: Coord -> [Coord]
adjacents coord@(r, c) = do
  coord' <-
    (,) <$> [r -1 .. r + 1]
      <*> [c -1 .. c + 1]
  coord' <$ guard (coord' /= coord)

pprWorld :: (Int, Int) -> World -> IO ()
pprWorld (rows, coys) (ts, ys) =
  forM_ [0 .. rows -1] \r -> do
    let render c
          | S.member coord ts = '|'
          | S.member coord ys = '#'
          | otherwise = '.'
          where
            coord = (r, c)
    putStrLn (fmap render [0 .. coys -1])

stepWorld :: (Int, Int) -> World -> World
stepWorld (rows, coys) w@(ts, ys) =
  packWorld $
    foldMap stepCoord $
      (,) <$> [0 .. rows -1] <*> [0 .. coys -1]
  where
    stepCoord coord
      | S.member coord ts =
        if yardCnt >= 3
          then mkYard coord
          else mkTree coord
      | S.member coord ys =
        if treeCnt >= 1 && yardCnt >= 1
          then mkYard coord
          else mempty
      | otherwise =
        if treeCnt >= 3
          then mkTree coord
          else mempty
      where
        (treeCnt, yardCnt) = bimap cnt cnt contribs
        cnt = fromMaybe 0 . (M.!? coord)

    mkContrib xs = M.fromListWith (+) do
      coord <- S.toList xs
      coord' <- adjacents coord
      pure (coord', 1 :: Int)

    contribs = bimap mkContrib mkContrib w

findFix :: M.Map World Int -> [(Int, World)] -> (Int, Int)
findFix seen ~((j, x) : xs) = case seen M.!? x of
  Nothing -> findFix (M.insert x j seen) xs
  Just i -> (i, j)

resourceValue :: World -> Int
resourceValue = uncurry (*) . bimap S.size S.size

instance Solution Day18 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- lines <$> getInputS
    let (dims, w) = parseFromRaw xs
        progression = iterate (stepWorld dims) w
        display = False
    do
      let wAns = progression !! 10
      when display do
        pprWorld dims wAns
      answerShow $ resourceValue wAns
    do
      let (i, j) = findFix M.empty (zip [0 ..] progression)
          period = j - i
          n = 1000000000
          i' = i + (n - i) `rem` period
          wAns = progression !! i'
      when display do
        pprWorld dims wAns
      answerShow $ resourceValue wAns
