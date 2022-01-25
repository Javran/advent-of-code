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
import qualified Data.PSQueue as PQ
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
  Gets the bound of the grid (rMax, cMax),
  and verifies that all coords within (inclusively) (0,0) ~ (rMax, cMax)
  are present and correct.
 -}
checkCompleteness :: [Coord] -> Maybe (Int, Int)
checkCompleteness xs = do
  (allCoords, Just (MinMax2D ((0, rMax), (0, cMax)))) <-
    pure $ foldMap (\c -> (S.singleton c, Just $ minMax2D c)) xs
  guard $
    allCoords == S.fromDistinctAscList do
      (,) <$> [0 .. rMax] <*> [0 .. cMax]
  pure (rMax, cMax)

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

  TODO: we are assuming no merge between two non-empty nodes can happen,
  we are not yet sure whether this is true.

 -}
estimateDist :: Coord -> Coord -> Int
estimateDist c e = if dist == 0 then 0 else (5 * dist - 4 + extra)
  where
    distCe = manhattan c e
    extra = if dist > 0 && distCe > 2 then distCe - 2 else 0
    dist = manhattan (0, 0) c

type SearchState = (Coord, Coord, M.Map Coord Int) -- location of the empty block, location of the goal block, and all current nodes.

aStar capacities (gScores :: M.Map SearchState Int) q0 = case PQ.minView q0 of
  Nothing -> error "queue exhausted"
  Just (((coord, targetNode, ns) :: SearchState) PQ.:-> (Arg fScore gScore), q1) ->
    if fScore == gScore
      then gScore
      else
        let nexts = do
              let used = 0
                  capa =
                    if ns M.! coord == 0
                      then capacities M.! coord
                      else error "empty grid is not empty?"
              coord' <- uldrOfCoord coord
              {-
                Clarification: we are "moving" the empty block
                from coord to coord', which means, in operational terms,
                moving data from coord' to coord.
               -}
              Just used' <- pure (ns M.!? coord')
              let newUsed' = used + used'
              guard $ newUsed' <= capa
              let gScore' = gScore + 1
                  targetNode' = if coord' == targetNode then coord else targetNode
                  fScore' = gScore' + estimateDist targetNode' coord'
                  ns' = M.insert coord newUsed' $ M.insert coord' 0 ns
                  ss' = (coord', targetNode', ns')
              guard case gScores M.!? ss' of
                Nothing -> True
                Just g -> gScore' < g
              pure (ss', fScore', gScore')
            gScores' =
              foldr
                (\(ss', _, gScore') -> M.insert ss' gScore')
                gScores
                nexts
            q2 =
              foldr
                (\(ss', fScore', gScore') -> PQ.insert ss' (Arg fScore' gScore'))
                q1
                nexts
         in aStar capacities gScores' q2

{-
  Some observation made after visualization (on my login input):

  - unique used values:

    [0,64,65,66,67,68,69,70,71,72,73,490,491,492,493,494,495,496,497,498,499]

    we can further break this down into: 0, 64~73, 490~499

  - unique total values:

    [85,86,87,88,89,90,91,92,93,94,501,502,503,504,505,506,507,508,509,510]

    which can be further broken down into: 85~94, 501~510

  few key thing:

  - 4xx used / 5xx total blocks might not be movable at all
  - for those that has a capacity of 85~94, data can move freely between
    them without worrying about moving target's capacity.

  TODO:
  this provides us with an interesting way of narrowing down the search space:

  - let's say:

    - all 4xx used / 5xx total becomes 490 used / 500 total
    - all x used / y total (where x in [64..73], y in [85..94] becomes
      just 70 / 80 (the choice is arbitrary).

  this might give us sufficiently low search space to work with.

 -}

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
    do
      let Just (yMax, xMax) = checkCompleteness $ fmap fst nodes
          [(theEmpty, _)] = filter ((== 0) . nUsed . snd) nodes
          target = (0, xMax)
          capacitiesPre :: M.Map Coord Int
          capacitiesPre = M.fromList $ (fmap . second) nSize nodes
          capacities = M.map tr capacitiesPre
            where
              tr v
                | v >= 85 && v <= 94 = 80
                | v >= 500 = 500
                | otherwise = error "unexpected capacity"

          initNs = M.fromList $ (fmap . second) (tr . nUsed) nodes
            where
              tr v
                | v == 0 = 0
                | v >= 64 && v <= 73 = 70
                | v >= 490 = 490
                | otherwise = error "unexpected used"
          initSs :: SearchState
          initSs = (theEmpty, target, initNs)
          r =
            aStar
              capacities
              (M.singleton initSs 0)
              (PQ.singleton initSs $ Arg (estimateDist target theEmpty) 0)
      forM_ [0 .. yMax] \y -> do
        let render x
              | (y, x) == target = 'G'
              | u == 0 = 'E'
              | u >= 400 = '#'
              | otherwise = '.'
              where
                u = initNs M.! (y, x)
        -- c = capacities M.! (y,x)
        putStrLn $ fmap render [0 .. xMax]
      print $ S.toAscList $ S.fromList $ M.elems initNs
      print $ S.toAscList $ S.fromList $ M.elems capacities
      answerShow r
