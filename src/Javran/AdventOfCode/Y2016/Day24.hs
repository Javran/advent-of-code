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

module Javran.AdventOfCode.Y2016.Day24
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import qualified Data.Array as Arr
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
import GHC.Generics (Generic)
import Javran.AdventOfCode.GridSystem.RowThenCol.Uldr
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day24 deriving (Generic)

{-

  We can probably apply the same simplification that we did in Y2019 Day 18 and 20,
  which is to prune useless tunnels and other terminal nodes
  (nodes of deg 1 or 2 that doesn't have tags attached)
  so that we can get to points of interest faster
  (search for `simplifyMapInfo` for details).

  After simplification, we can priority-search to find the shortest path
  (with current path length as priority).

  TODO: we need some more cleanups, more specifically, we should only consider
  numbered nodes in the map - probably we can run shortest path algorithms
  on all numbered nodes to all numbered nodes (but stop at those numbered nodes
  so that we only construct direct paths rather than allowing indirect paths.

  Then priority search will then use search state (<cur node>, <not-yet-visited-nodes>).

 -}

data MapInfo = MapInfo
  { miStart :: Coord
  , miGraph :: M.Map Coord [Coord]
  , miDist :: M.Map (MinMax Coord) Int
  , miNums :: M.Map Coord Int
  , miDims :: (Int, Int)
  }
  deriving (Show)

parseFromRaw :: [String] -> MapInfo
parseFromRaw xs = MapInfo {miGraph, miDist, miStart, miNums, miDims}
  where
    rows = length xs
    cols = length (head xs)
    miDims = (rows, cols)
    bds = ((0, 0), (rows -1, cols -1))
    rawMap = Arr.array bds do
      (r, rs) <- zip [0 ..] xs
      (c, x) <- zip [0 ..] rs
      pure ((r, c), x)
    gPre = do
      r <- [0 .. rows -1]
      c <- [0 .. cols -1]
      let coord = (r, c)
          ch = rawMap Arr.! coord
      guard $ ch /= '#'
      let vs = do
            coord' <- uldrOfCoord coord
            guard $ inRange bds coord'
            guard $ rawMap Arr.! coord' /= '#'
            pure coord'
      pure (coord, ch, vs)
    miGraph = M.fromList $ fmap (\(c, _, vs) -> (c, vs)) gPre
    miDist = M.fromList $ do
      (u, _, vs) <- gPre
      v <- vs
      guard $ u <= v
      pure (minMaxFromPair (u, v), 1)
    nums@((0, miStart) : _) = sortOn fst do
      (u, ch, _) <- gPre
      guard $ isDigit ch
      pure (ord ch - ord '0', u)
    miNums = M.fromList $ fmap swap nums

simplifyMapInfo :: MapInfo -> MapInfo
simplifyMapInfo mi@MapInfo {miGraph, miStart, miNums} = simplifyMapInfoAux shouldKeep mi $ PQ.fromList do
  (coord, cs) <- M.toList miGraph
  let deg = length cs
  guard $ deg <= 2
  pure (coord PQ.:-> deg)
  where
    shouldKeep = M.keysSet miNums

getDist :: M.Map (MinMax Coord) Int -> (Coord, Coord) -> Maybe Int
getDist m p = m M.!? minMaxFromPair p

simplifyMapInfoAux :: S.Set Coord -> MapInfo -> PQ.PSQ Coord Int -> MapInfo
simplifyMapInfoAux
  shouldKeep
  mi@MapInfo {miGraph, miDist}
  q0 = case PQ.minView q0 of
    Nothing -> mi
    Just (c PQ.:-> deg, q1) ->
      if S.member c shouldKeep
        then simplifyMapInfoAux shouldKeep mi q1
        else case deg of
          0 ->
            let miGraph' = M.delete c miGraph
             in simplifyMapInfoAux shouldKeep mi {miGraph = miGraph'} q1
          1 ->
            let [c'] = miGraph M.! c
                miGraph' = M.adjust (delete c) c' $ M.delete c miGraph
                q2 = PQ.insert c' (length $ miGraph' M.! c') q1
             in simplifyMapInfoAux shouldKeep mi {miGraph = miGraph'} q2
          2 ->
            let [c1, c2] = miGraph M.! c
                mOldDist = getDist miDist (c1, c2)
                newDist = fromJust (getDist miDist (c, c1)) + fromJust (getDist miDist (c, c2))
                safeToPrune = case mOldDist of
                  Nothing -> True
                  Just oldDist -> newDist <= oldDist
                miGraph' =
                  M.adjust ((c1 :) . delete c) c2 $
                    M.adjust ((c2 :) . delete c) c1 $
                      M.delete c miGraph
                miDist' =
                  -- probably not worth removing old ones
                  M.insert (minMaxFromPair (c1, c2)) newDist miDist
                enqueue cx = PQ.insert cx (length $ miGraph' M.! cx)
                q2 = enqueue c1 . enqueue c2 $ q1
             in if safeToPrune
                  then simplifyMapInfoAux shouldKeep mi {miGraph = miGraph', miDist = miDist'} q2
                  else simplifyMapInfoAux shouldKeep mi q1
          _
            | deg >= 3 ->
              -- meaning all deg 1 and 2 are done.
              mi
          _ -> unreachable

{-
  Basically just single-source shortest distance algorithm
  but not allowing further expansion from a target node.
  by doing so we only consider direct paths
  (a path that does not go through a node of interest)
 -}
shortestDirectDists mi@MapInfo {miGraph, miDist} targets dists q0 = case PQ.minView q0 of
  Nothing -> M.restrictKeys dists targets
  Just (u PQ.:-> distU, q1) ->
    let nexts = do
          -- not allowed to expand further if we have reached some targets.
          guard $ S.notMember u targets
          Just vs <- pure (miGraph M.!? u)
          v <- vs

          let distV' = distU + fromJust (getDist miDist (u, v))
              mDistV = dists M.!? v
          guard $ maybe True (distV' <) mDistV
          pure (v, distV')
        dists' = foldr (\(v, distV') -> M.insert v distV') dists nexts
        q2 = foldr (\(v, distV') -> PQ.insert v distV') q1 nexts
     in shortestDirectDists mi targets dists' q2

type SearchState = (Int, IS.IntSet) -- current node, nodes to be visited

solve (simpleDists :: IM.IntMap [(Int, Int)]) (pathLens :: M.Map SearchState Int) q0 = case PQ.minView q0 of
  Nothing -> error "queue exhausted"
  Just (((u, todos) :: SearchState) PQ.:-> len, q1) ->
    if IS.null todos
      then len
      else let nexts = do
                 Just vs <- pure $ simpleDists IM.!? u
                 (v, d) <- vs
                 let todos' = IS.delete v todos
                     len' = len + d
                     next = (v, todos')
                 guard $ maybe True (len' <) $ pathLens M.!? next
                 pure (next, len')
               pathLens' = foldr (\(next, len') -> M.insert next len') pathLens nexts
               q2 = foldr (\(next, prio) -> PQ.insert next prio) q1 nexts
         in solve simpleDists pathLens' q2

instance Solution Day24 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    mi <- parseFromRaw . lines <$> getInputS
    let mi'@MapInfo {miDims = (rows, cols)} = simplifyMapInfo mi
    putStrLn $ "before vs after: " <> show (M.size (miGraph mi), M.size (miGraph mi'))
    let shouldPrintMap = False
    when shouldPrintMap do
      forM_ [0 .. rows -1] \r -> do
        let render c =
              case miGraph mi M.!? (r, c) of
                Nothing -> '#'
                Just _ ->
                  if
                      | coord == miStart mi -> 'S'
                      | Just t <- miNums mi M.!? coord -> chr (ord '0' + t)
                      | otherwise ->
                        case miGraph mi' M.!? (r, c) of
                          Nothing -> ' '
                          Just _ -> '.'
              where
                coord = (r, c)

        putStrLn $ fmap render [0 .. cols -1]
    let points = S.insert (miStart mi') (M.keysSet $ miNums mi')
        ptToInt = (miNums mi' M.!)
        simpleDists :: IM.IntMap [(Int, Int)]
        simpleDists = IM.fromList do
          src <- S.toList points
          let targets = S.delete src points
              dists = shortestDirectDists mi' targets M.empty (PQ.singleton src 0)
          pure (ptToInt src, (fmap . first ) ptToInt $ M.toList dists)
        initSt = (0, initTodos)
        initTodos = IS.delete 0 $ IS.fromList $ M.elems $ miNums mi
    answerShow $ solve simpleDists (M.singleton initSt 0) (PQ.singleton initSt 0)
