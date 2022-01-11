{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2017.Day3
  (
  )
where

import Control.Monad
import Control.Monad.Loops
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import Javran.AdventOfCode.Prelude

data Day3 deriving (Generic)

type Coord = (Int, Int) -- row, col

data Dir = U | D | L | R

applyDir :: Dir -> Coord -> Coord
applyDir = \case
  U -> first pred
  D -> first succ
  L -> second pred
  R -> second succ

spiral :: [Dir]
spiral = concat $ zipWith (\d t -> replicate t d) dirs times
  where
    times = concatMap (\i -> [i, i]) [1 ..]
    dirs = cycle [R, U, L, D]

spiralCoords :: [Coord]
spiralCoords = scanl (\coord d -> applyDir d coord) (0, 0) spiral

adjacents :: Coord -> [Coord]
adjacents coord@(r, c) = do
  r' <- [r -1 .. r + 1]
  c' <- [c - 1 .. c + 1]
  let coord' = (r', c')
  coord' <$ guard (coord' /= coord)

genUntil :: Int -> State ([Coord], M.Map Coord Int) (Maybe Int)
genUntil n = do
  (~(c : cs), m) <- get
  let v = sum $ mapMaybe (m M.!?) (adjacents c)
  if v > n
    then pure (Just v)
    else Nothing <$ put (cs, M.insert c v m)

instance Solution Day3 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    n <- read @Int . head . lines <$> getInputS
    answerShow (manhattan (0, 0) $ spiralCoords !! (n -1))
    answerShow $
      evalState
        (untilJust (genUntil n))
        (tail spiralCoords, M.singleton (0, 0) 1)
