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

module Javran.AdventOfCode.Y2019.Day20
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import qualified Data.Array as Arr
import qualified Data.Array.IArray as IArr
import qualified Data.Array.ST as Arr
import Data.Bifunctor
import Data.Bool
import Data.Char
import Data.Either
import Data.Foldable
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Ord
import qualified Data.PSQueue as PQ
import Data.Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Debug.Trace
import GHC.Generics (Generic)
import GHC.IO (unsafePerformIO)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day20 deriving (Generic)

type Coord = (Int, Int)

data MapInfo = MapInfo
  { miGraph :: M.Map Coord (S.Set Coord)
  , miStartEnd :: (Coord, Coord)
  , miDist :: M.Map (Coord, Coord) Int
  }

mkMapInfo :: Arr.Array Coord Char -> MapInfo
mkMapInfo rawFloor =
  MapInfo
    { miStartEnd = (startCoord, endCoord)
    , miGraph
    , miDist = M.fromList do
        (u, vs) <- M.toList miGraph
        v <- S.toList vs
        guard $ u <= v
        pure ((u, v), 1)
    }
  where
    ([startCoord], [endCoord]) =
      ( portals M.! "AA"
      , portals M.! "ZZ"
      )
    getCell coord = rawFloor Arr.! coord
    portals :: M.Map String [Coord]
    portals = M.unionsWith (<>) pPre
    miGraph :: M.Map Coord (S.Set Coord)
    miGraph = M.mapWithKey connectPortal miGraphPre
      where
        connectPortal :: Coord -> [Either String Coord] -> S.Set Coord
        connectPortal coord vs = S.fromList (ls <> rs)
          where
            (lsPre, rs) = partitionEithers vs
            ls =
              mapMaybe
                (\tag -> do
                   guard $ tag `notElem` ["AA", "ZZ"]
                   let [c'] = (portals M.! tag) \\ [coord]
                   pure c')
                lsPre

    miGraphPre :: M.Map Coord [Either String Coord]
    miGraphPre = M.unionsWith (<>) gPre
    (gPre, pPre) = unzip do
      (coord@(r, c), '.') <- Arr.assocs rawFloor
      let tryDir coord0 p0 p1 = do
            let v = getCell coord0
            case v of
              '#' -> []
              '.' -> pure $ (Right coord0, Nothing)
              _
                | isAsciiUpper v -> do
                  let pTag = [getCell p0, getCell p1]
                  pure $ (Left pTag, Just pTag)
              _ -> errInvalid
      (val, mPortal) <-
        asum
          [ -- up
            tryDir (r -1, c) (r -2, c) (r -1, c)
          , -- down
            tryDir (r + 1, c) (r + 1, c) (r + 2, c)
          , -- left
            tryDir (r, c -1) (r, c -2) (r, c -1)
          , --right
            tryDir (r, c + 1) (r, c + 1) (r, c + 2)
          ]
      pure
        ( M.singleton coord [val] -- one piece of miGraph
        , -- portal tag to this coord.
          maybe
            M.empty
            (\tag -> M.singleton tag [coord])
            mPortal
        )

debugMapInfo :: MapInfo -> IO ()
debugMapInfo MapInfo {miGraph, miStartEnd = (startCoord, endCoord), miDist} = do
  let Just (MinMax2D ((minR, maxR), (minC, maxC))) =
        foldMap (Just . minMax2D) $ M.keys miGraph
  forM_ [minR - 1 .. maxR + 1] $ \r -> do
    let render c =
          if
              | coord == startCoord -> 'S'
              | coord == endCoord -> 'E'
              | minR <= r && r <= maxR && minC <= c && c <= maxC ->
                case miGraph M.!? coord of
                  Nothing -> '█'
                  Just _ -> '.'
              | otherwise -> ' '
          where
            coord = (r, c)
    putStrLn (fmap render [minC - 2 .. maxC + 2])
  let showRoutes = True
  when showRoutes do
    c <- forM (M.toAscList miDist) $ \((c0, c1), dist) -> do
      if M.member c0 miGraph && M.member c1 miGraph
        then do
          putStrLn $
            show c0
              <> " <=> "
              <> show c1
              <> ": "
              <> show dist
          pure (1 :: Int)
        else pure 0
    print $ sum c

runSpfa :: MapInfo -> (Maybe Int, M.Map Coord Int)
runSpfa MapInfo {miStartEnd = (startCoord, endCoord), miGraph, miDist} =
  runState (spfaWith (PQ.singleton startCoord 0)) (M.singleton startCoord 0)
  where
    spfaWith q0 =
      case PQ.minView q0 of
        Nothing ->
          gets (M.!? endCoord)
        Just (u PQ.:-> distU, q1) -> do
          performEnqs <- forM (S.toList (miGraph M.! u)) $ \v -> do
            let distV' = distU + fromJust (getDist miDist (u, v))
            mDistV <- gets (M.!? v)
            if maybe True (distV' <) mDistV
              then do
                modify (M.insert v distV')
                pure
                  (PQ.alter
                     (\case
                        Nothing -> Just distV'
                        Just distV -> Just (min distV distV'))
                     v)
              else pure id
          spfaWith $ appEndo (foldMap Endo performEnqs) q1

simplifyMapInfo :: MapInfo -> MapInfo
simplifyMapInfo mi@MapInfo {miGraph} = simplifyMapInfoAux mi $ PQ.fromList do
  (coord, cs) <- M.toList miGraph
  let deg = S.size cs
  guard $ deg <= 2
  pure (coord PQ.:-> deg)

-- TODO: we should probably have an util module that deal with this kind of things for undirected graphs.
getDist :: M.Map (Coord, Coord) Int -> (Coord, Coord) -> Maybe Int
getDist m (a, b) = m M.!? if a <= b then (a, b) else (b, a)

{-
  Basically the same simplification process as in day 18.
 -}
simplifyMapInfoAux :: MapInfo -> PQ.PSQ Coord Int -> MapInfo
simplifyMapInfoAux
  mi@MapInfo {miGraph, miDist, miStartEnd = (startCoord, endCoord)}
  q0 = case PQ.minView q0 of
    Nothing -> mi
    Just (c PQ.:-> deg, q1) ->
      if c == startCoord || c == endCoord
        then simplifyMapInfoAux mi q1
        else case deg of
          1 ->
            let [c'] = S.toList (miGraph M.! c)
                miGraph' = M.adjust (S.delete c) c' $ M.delete c miGraph
                q2 = PQ.insert c' (S.size $ miGraph' M.! c') q1
             in simplifyMapInfoAux mi {miGraph = miGraph'} q2
          2 ->
            let [c1, c2] = S.toList (miGraph M.! c)
                mOldDist = getDist miDist (c1, c2)
                newDist = fromJust (getDist miDist (c, c1)) + fromJust (getDist miDist (c, c2))
                safeToPrune = case mOldDist of
                  Nothing -> True
                  Just oldDist -> newDist <= oldDist
                miGraph' =
                  M.adjust (S.insert c1 . S.delete c) c2 $
                    M.adjust (S.insert c2 . S.delete c) c1 $
                      M.delete c miGraph
                miDist' =
                  let p = if c1 <= c2 then (c1, c2) else (c2, c1)
                   in -- probably not worth removing old ones
                      M.insert p newDist miDist
                enqueue cx = PQ.insert cx (S.size $ miGraph' M.! cx)
                q2 = enqueue c1 . enqueue c2 $ q1
             in if safeToPrune
                  then simplifyMapInfoAux mi {miGraph = miGraph', miDist = miDist'} q2
                  else simplifyMapInfoAux mi q1
          _
            | deg >= 3 ->
              -- meaning all deg 1 and 2 are done.
              mi
          _ -> unreachable

instance Solution Day20 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- lines <$> getInputS
    let rows = length xs
        cols = length (head xs)
        rawFloor :: Arr.Array Coord Char
        rawFloor = Arr.array
          ((0, 0), (rows -1, cols -1))
          do
            (r, rs) <- zip [0 ..] xs
            (c, x) <- zip [0 ..] rs
            pure ((r, c), x)
        mi = mkMapInfo rawFloor
        mi' = simplifyMapInfo mi
        (Just endDist, _) = runSpfa mi'
    answerShow endDist
