{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Javran.AdventOfCode.GridSystem.RowThenCol.Nwse
  ( Dir (.., North, West, South, East)
  , Coord
  , applyDir
  , oppositeDir
  , turnLeft
  , turnRight
  , nwseOfCoord
  , allDirs
  )
where

import Data.Bifunctor

type Coord = (Int, Int) -- row then col

data Dir = N | W | S | E deriving (Eq, Ord, Enum, Show, Bounded)

pattern North, West, South, East :: Dir
pattern North = N
pattern West = W
pattern South = S
pattern East = E

{-# COMPLETE North, West, South, East #-}

applyDir :: Dir -> Coord -> Coord
applyDir = \case
  N -> first (\r -> r - 1)
  S -> first (\r -> r + 1)
  W -> second (\c -> c -1)
  E -> second (\c -> c + 1)
{-# INLINEABLE applyDir #-}

oppositeDir :: Dir -> Dir
oppositeDir = \case
  N -> S
  S -> N
  W -> E
  E -> W
{-# INLINEABLE oppositeDir #-}

turnLeft :: Dir -> Dir
turnLeft = \case
  N -> W
  W -> S
  S -> E
  E -> N
{-# INLINEABLE turnLeft #-}

turnRight :: Dir -> Dir
turnRight = \case
  N -> E
  E -> S
  S -> W
  W -> N
{-# INLINEABLE turnRight #-}

nwseOfCoord :: Coord -> [Coord]
nwseOfCoord (r, c) = [(r -1, c), (r, c -1), (r + 1, c), (r, c + 1)]
{-# INLINEABLE nwseOfCoord #-}

allDirs :: [Dir]
allDirs = [N, W, S, E]
{-# INLINEABLE allDirs #-}
