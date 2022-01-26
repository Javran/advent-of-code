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
  TODO:

  We can probably apply the same simplification that we did in Y2019 Day 18 and 20,
  which is to prune useless tunnels and other terminal nodes
  (nodes of deg 1 or 2 that doesn't have tags attached)
  so that we can get to points of interest faster
  (search for `simplifyMapInfo` for details).

  After simplification, we can priority-search to find the shortest path
  (with current path length as priority).

 -}

data MapInfo = MapInfo
  { miStart :: Coord
  , miGraph :: M.Map Coord [Coord]
  , miDist :: M.Map (MinMax Coord) Int
  , miVisits :: S.Set Coord
  , miDims :: (Int, Int)
  }
  deriving (Show)

parseFromRaw :: [String] -> MapInfo
parseFromRaw xs = MapInfo {miGraph, miDist, miStart, miVisits, miDims}
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
    (0, miStart) : visits = sortOn fst do
      (u, ch, _) <- gPre
      guard $ isDigit ch
      pure (ord ch - ord '0', u)
    miVisits = S.fromList $ fmap snd visits

simplifyMapInfo :: MapInfo -> MapInfo
simplifyMapInfo mi@MapInfo {miGraph, miStart, miVisits} = simplifyMapInfoAux shouldKeep mi $ PQ.fromList do
  (coord, cs) <- M.toList miGraph
  let deg = length cs
  guard $ deg <= 2
  pure (coord PQ.:-> deg)
  where
    shouldKeep = S.insert miStart miVisits

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

instance Solution Day24 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    mi <- parseFromRaw . lines <$> getInputS
    let mi'@MapInfo {miDims = (rows, cols)} = simplifyMapInfo mi
    putStrLn $ "before vs after: " <> show (M.size (miGraph mi), M.size (miGraph mi'))
    forM_ [0 .. rows -1] \r -> do
      let render c =
            case miGraph mi M.!? (r, c) of
              Nothing -> '#'
              Just _ ->
                if
                    | coord == miStart mi -> 'S'
                    | S.member coord (miVisits mi) -> 'V'
                    | otherwise ->
                      case miGraph mi' M.!? (r, c) of
                        Nothing -> ' '
                        Just _ -> '.'
            where
              coord = (r, c)

      putStrLn $ fmap render [0 .. cols -1]
