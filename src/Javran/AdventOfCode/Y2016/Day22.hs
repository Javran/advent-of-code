{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2016.Day22
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Debug.Trace
import GHC.Generics (Generic)
import Javran.AdventOfCode.GridSystem.RowThenCol.Uldr
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day22 deriving (Generic)

data Node = Node
  { nSize :: Int
  , nUsed :: Int
  , nAvail :: Int
  , nUsePercent :: Int
  }
  deriving (Show)

nodeP :: ReadP (Coord, Node)
nodeP = do
  strP "/dev/grid/node-x"
  x <- decimal1P
  strP "-y"
  y <- decimal1P <* skipSpaces
  nSize <- decimal1P <* charP 'T' <* skipSpaces
  nUsed <- decimal1P <* charP 'T' <* skipSpaces
  nAvail <- decimal1P <* charP 'T' <* skipSpaces
  nUsePercent <- decimal1P <* charP '%'
  {-
    Coord order is y-then-x,
    so that it's consistent with row-then-col grid system.
   -}
  pure ((y, x), Node {nSize, nUsed, nAvail, nUsePercent})

{-
  Gets the dimension of the grid (rows, cols),
  and verifies that all coords within (inclusively) (0,0) ~ (rows-1, cols-1)
  are present and correct.
 -}
checkCompleteness :: [Coord] -> Maybe (Int, Int)
checkCompleteness xs = do
  (allCoords, Just (MinMax2D ((0, rMax), (0, cMax)))) <-
    pure $ foldMap (\c -> (S.singleton c, Just $ minMax2D c)) xs
  guard $
    allCoords == S.fromDistinctAscList do
      (,) <$> [0 .. rMax] <*> [0 .. cMax]
  pure (rMax + 1, cMax + 1)

{-
  TODO: I don't know if my assumption would hold, but let's see.

  This heuristic is based on the fact that for every move that
  brings goal block closer to the origin (except for the last step
  the brings goal block to the origin)
  it is accompanied by shuffling the empty block around so that
  we can advance the goal block again.

  Let [g] be goal block, [e] empty block, [x] an occupied block:

  0: [x] [e]-[g]

  1: [x] [g] [e]
              |
             [x]

  2: [x] [g] [x]

         [x]-[e]

  3: [x] [g] [x]

     [x]-[e] [x]

  4: [x] [g] [x]
      |
     [e] [x] [x]

  5: [e] [g] [x]

     [x] [x] [x]

  Therefore, if we need `dist` steps moving goal block to the origin
  pretending all other blocks are empty, we need `dist` moves.
  If dist > 0, it is also accompanied by `4*(dist-1)` extra moves
  for shuffling the empty block around.

  dist + 4 * (dist - 1) (dist > 0)
  ==> 5 * dist - 4

  Note that so far we haven't take care of where the empty block is,
  but it's obvious that we need to bring the block closer to the goal
  block to make any progress, I'm making a random guess that
  if the dist between empty block and target block is > 2
  (as the empty block shuffling process never brings empty block
  further than 2 to the goal block), we include an extra distance of:

  `dist(empty, goal) - 2`

  in our heuristic.

 -}
estimateDist :: Coord -> Coord -> Int
estimateDist c e = if dist == 0 then 0 else (5 * dist - 4 + extra)
  where
    distCe = manhattan c e
    extra = if dist > 0 && distCe > 2 then distCe - 2 else 0
    dist = manhattan (0, 0) c

instance Solution Day22 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    nodes <- fmap (consumeOrDie nodeP) . drop 2 . lines <$> getInputS
    let viablePairs = do
          ((ca, a), xs0) <- pick nodes
          (cb, b) <- xs0
          guard $ nUsed a /= 0 && nUsed a <= nAvail b
          pure (ca, cb)
    answerShow $ length viablePairs
    {-
      Few notes:
      - the grid has all coords present inside the rectangle,
        this should be a reasonable assumption to make.
      - for my specific input, there is only one empty space
        that we can use to move things around, suggesting
        that initial few moves might be quite restricted that
        a plain search (probably a* algorithm) would work.
     -}
    print (checkCompleteness $ fmap fst nodes)
    print $ do
      (c, n) <- nodes
      guard $ nUsed n < 64
      pure c

    print do
      (u, v) <- viablePairs
      guard $ manhattan u v == 1
      pure (u, v)
