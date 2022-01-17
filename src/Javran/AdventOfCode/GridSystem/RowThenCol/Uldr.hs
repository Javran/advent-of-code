{-# LANGUAGE LambdaCase #-}

module Javran.AdventOfCode.GridSystem.RowThenCol.Uldr
  ( Dir (..)
  , Coord
  , applyDir
  , oppositeDir
  , turnLeft
  , turnRight
  , uldrOfCoord
  , allDirs
  )
where

import Data.Bifunctor

type Coord = (Int, Int) -- row then col

data Dir = U | L | D | R deriving (Eq, Ord, Enum, Show)

applyDir :: Dir -> Coord -> Coord
applyDir = \case
  U -> first (\r -> r - 1)
  D -> first (\r -> r + 1)
  L -> second (\c -> c -1)
  R -> second (\c -> c + 1)
{-# INLINEABLE applyDir #-}

oppositeDir :: Dir -> Dir
oppositeDir = \case
  U -> D
  D -> U
  L -> R
  R -> L
{-# INLINEABLE oppositeDir #-}

turnLeft :: Dir -> Dir
turnLeft = \case
  U -> L
  L -> D
  D -> R
  R -> U
{-# INLINEABLE turnLeft #-}

turnRight :: Dir -> Dir
turnRight = \case
  U -> R
  R -> D
  D -> L
  L -> U
{-# INLINEABLE turnRight #-}

uldrOfCoord :: Coord -> [Coord]
uldrOfCoord (r, c) = [(r -1, c), (r, c -1), (r + 1, c), (r, c + 1)]
{-# INLINEABLE uldrOfCoord #-}

allDirs :: [Dir]
allDirs = [U, L, D, R]
{-# INLINEABLE allDirs #-}
