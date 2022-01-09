{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2018.Day22
  (
  )
where

import Control.Applicative
import Control.Monad
import Data.Function.Memoize (memoFix)
import qualified Data.Map.Strict as M
import qualified Data.PSQueue as PQ
import Data.Semigroup
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day22 deriving (Generic)

type Coord = (Int, Int) -- x, y

type Input = (Int, Coord)

data RegionType = Rocky | Wet | Narrow deriving (Enum)

inputP :: ReadP Input
inputP =
  (,) <$> (string "depth: " *> decimal1P) <*> do
    _ <- string "\ntarget: "
    x <- decimal1P <* char ','
    y <- decimal1P <* char '\n'
    pure (x, y)

mkGeologicIndex :: Input -> Coord -> Int
mkGeologicIndex (depth, target) = memoFix \q coord@(x, y) ->
  if
      | coord == (0, 0) -> 0
      | coord == target -> 0
      | y == 0 -> x * 16807
      | x == 0 -> y * 48271
      | otherwise ->
        let erosionLevel' coord' = (q coord' + depth) `rem` 20183
         in erosionLevel' (x -1, y) * erosionLevel' (x, y -1)

pprRegion :: (Int, Int) -> (Coord -> Int) -> IO ()
pprRegion (maxX, maxY) riskLevel = do
  forM_ [0 .. maxY] \y -> do
    let render x = case riskLevel (x, y) of
          0 -> '.'
          1 -> '='
          2 -> '|'
          _ -> unreachable
    putStrLn (fmap render [0 .. maxX])

data Tool = Torch | ClimbingGear | Neither deriving (Eq, Ord, Enum, Bounded, Show)

type SearchState = (Coord, Tool)

nextStates :: (Coord -> Int) -> SearchState -> [(Int, SearchState)]
nextStates riskLevel (coord, tool) =
  (do
     tool' <- universe @Tool
     guard $ tool' /= tool
     let s = (coord, tool')
     (7, s) <$ guard (isCompatible s))
    <|> (do
           coord'@(x', y') <- udlrOfCoord coord
           guard $ x' >= 0 && y' >= 0
           let s = (coord', tool)
           (1, s) <$ guard (isCompatible s))
  where
    isCompatible (c, t) = case riskLevel c of
      0 -> t /= Neither
      1 -> t /= Torch
      ~2 -> t /= ClimbingGear

estimateDist :: SearchState -> SearchState -> Int
estimateDist ((a, b), t0) ((c, d), t1) = abs (a - c) + abs (b - d) + if t0 == t1 then 0 else 7

aStar :: (Coord -> Int) -> SearchState -> PQ.PSQ SearchState (Arg Int Int) -> M.Map SearchState Int -> Int
aStar riskLevel goal q0 dists = case PQ.minView q0 of
  Nothing -> error "queue exhausted"
  Just (u PQ.:-> (Arg _fScore distU), q1) ->
    if u == goal
      then distU
      else
        let nexts = do
              (delta, v) <- nextStates riskLevel u
              let mDistV = dists M.!? v
                  distV' = distU + delta
                  fScore' = distV' + estimateDist v goal
              guard $ maybe True (distV' <) mDistV
              pure (v, distV', Arg fScore' distV')
            q2 = foldr upd q1 nexts
              where
                upd (v, _, prio') = PQ.insert v prio'
            dists' = foldr upd dists nexts
              where
                upd (v, distV', _) = M.insert v distV'
         in aStar riskLevel goal q2 dists'

instance Solution Day22 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    inp@(depth, targetCoord) <- consumeOrDie inputP <$> getInputS
    let geologicIndex = mkGeologicIndex inp
        erosionLevel coord = (geologicIndex coord + depth) `rem` 20183
        riskLevel coord = erosionLevel coord `rem` 3
        display = False
    when display do
      pprRegion targetCoord riskLevel
    answerShow $ sum do
      let (_, (targetX, targetY)) = inp
      x <- [0 .. targetX]
      y <- [0 .. targetY]
      pure $ riskLevel (x, y)
    do
      let initSt = ((0, 0), Torch)
          goalSt = (targetCoord, Torch)
          est = estimateDist initSt goalSt
      answerShow $
        aStar
          riskLevel
          goalSt
          (PQ.singleton initSt (Arg est 0))
          (M.singleton initSt 0)
