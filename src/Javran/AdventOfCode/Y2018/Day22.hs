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

data RegionInfo = RegionInfo
  { geologicIndex :: Coord -> Int
  , erosionLevel :: Coord -> Int
  , riskLevel :: Coord -> Int
  , regionType :: Coord -> RegionType
  }

mkRegionInfo :: Input -> RegionInfo
mkRegionInfo (depth, target) =
  RegionInfo
    { geologicIndex
    , erosionLevel
    , riskLevel
    , regionType
    }
  where
    erosionLevel' qGeologicInd coord' =
      (qGeologicInd coord' + depth) `rem` 20183
    geologicIndex = memoFix \q coord@(x, y) ->
      if
          | coord == (0, 0) -> 0
          | coord == target -> 0
          | y == 0 -> x * 16807
          | x == 0 -> y * 48271
          | otherwise ->
            erosionLevel' q (x -1, y) * erosionLevel' q (x, y -1)
    erosionLevel = erosionLevel' geologicIndex
    riskLevel coord = erosionLevel coord `rem` 3
    regionType = toEnum . riskLevel

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

nextStates :: RegionInfo -> SearchState -> [(Int, SearchState)]
nextStates RegionInfo {regionType} (coord, tool) =
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
    isCompatible (c, t) = case regionType c of
      Rocky -> t /= Neither
      Wet -> t /= Torch
      Narrow -> t /= ClimbingGear

{-
  An underestimation between SearchStates,
  which is just manhattan distance. Plus tool switching cost if tools differ.
 -}
estimateDist :: SearchState -> SearchState -> Int
estimateDist ((a, b), t0) ((c, d), t1) =
  abs (a - c) + abs (b - d) + if t0 == t1 then 0 else 7

aStar
  :: RegionInfo
  -> SearchState
  -> PQ.PSQ SearchState (Arg Int Int)
  -> M.Map SearchState Int
  -> Int
aStar ri goal = fix \search q0 dists -> case PQ.minView q0 of
  Nothing -> error "queue exhausted"
  Just (u PQ.:-> (Arg _fScore distU), q1) ->
    if u == goal
      then distU
      else
        let nexts = do
              (delta, v) <- nextStates ri u
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
         in search q2 dists'

instance Solution Day22 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    inp@(_, targetCoord) <- consumeOrDie inputP <$> getInputS
    let ri@RegionInfo {riskLevel} = mkRegionInfo inp
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
          ri
          goalSt
          (PQ.singleton initSt (Arg est 0))
          (M.singleton initSt 0)
