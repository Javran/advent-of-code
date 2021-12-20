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

data Dir = U | D | L | R deriving (Eq)

data ParsedMap = ParsedMap
  { pmGraph :: M.Map Coord [Either String Coord]
  , pmPortals :: M.Map String [(Coord, Dir)]
  , pmStartEnd :: (Coord, Coord)
  , pmInnerOuter :: (MinMax2D Int Int, MinMax2D Int Int)
  }

parseMap :: Arr.Array Coord Char -> ParsedMap
parseMap rawFloor =
  ParsedMap
    { pmGraph
    , pmStartEnd = (startCoord, endCoord)
    , pmPortals
    , pmInnerOuter = (inner, outer)
    }
  where
    getCell coord = rawFloor Arr.! coord
    ([(startCoord, _)], [(endCoord, _)]) =
      ( pmPortals M.! "AA"
      , pmPortals M.! "ZZ"
      )
    pmGraph = M.unionsWith (<>) gPre
    pmPortals = M.unionsWith (<>) pPre
    Just outer = foldMap (Just . minMax2D) do
      (coord, x) <- Arr.assocs rawFloor
      guard $ x `elem` "#."
      pure coord

    Just inner = foldMap (Just . minMax2D) do
      let isInside =
            let MinMax2D ((minR, maxR), (minC, maxC)) = outer
             in inRange ((minR, minC), (maxR, maxC))
      (coord, ' ') <- Arr.assocs rawFloor
      guard $ isInside coord
      pure coord

    (gPre, pPre) = unzip do
      (coord@(r, c), '.') <- Arr.assocs rawFloor
      let tryDir dir coord0 p0 p1 = do
            let v = getCell coord0
            case v of
              '#' -> []
              '.' -> pure $ (Right coord0, Nothing)
              _
                | isAsciiUpper v -> do
                  let pTag = [getCell p0, getCell p1]
                  pure $ (Left pTag, Just (pTag, dir))
              _ -> errInvalid
      (val, mPortal) <-
        asum
          [ tryDir U (r -1, c) (r -2, c) (r -1, c)
          , tryDir D (r + 1, c) (r + 1, c) (r + 2, c)
          , tryDir L (r, c -1) (r, c -2) (r, c -1)
          , tryDir R (r, c + 1) (r, c + 1) (r, c + 2)
          ]
      pure
        ( M.singleton coord [val] -- one piece of miGraph
        , -- portal tag to this coord.
          maybe
            M.empty
            (\(tag, dir) -> M.singleton tag [(coord, dir)])
            mPortal
        )

data PortalSide = PsInner | PsOuter

type Edges = M.Map Coord (Maybe (String, PortalSide))

data MapInfo = MapInfo
  { miGraph :: M.Map Coord Edges
  , miStartEnd :: (Coord, Coord)
  , miDist :: M.Map (MinMax Coord) Int
  , miInnerOuter :: (MinMax2D Int Int, MinMax2D Int Int)
  }

mkMapInfo :: ParsedMap -> MapInfo
mkMapInfo
  ParsedMap
    { pmGraph
    , pmStartEnd = miStartEnd
    , pmPortals
    , pmInnerOuter = miInnerOuter
    } =
    MapInfo
      { miStartEnd
      , miInnerOuter
      , miGraph
      , miDist = M.fromList do
          (u, vs) <- M.toList miGraph
          v <- M.keys vs
          guard $ u <= v
          pure (minMaxFromPair (u, v), 1)
      }
    where
      isOnInnerRim :: Coord -> Bool
      isOnInnerRim = inRange ((minR -1, minC -1), (maxR + 1, maxC + 1))
        where
          (MinMax2D ((minR, maxR), (minC, maxC)), _Outer) = miInnerOuter
      miGraph :: M.Map Coord Edges
      miGraph = M.mapWithKey connectPortal miGraphPre
        where
          connectPortal :: Coord -> [Either String Coord] -> Edges
          connectPortal coord vs = M.fromList (ls <> fmap (,Nothing) rs)
            where
              (lsPre, rs) = partitionEithers vs
              ls :: [(Coord, Maybe (String, PortalSide))]
              ls =
                mapMaybe
                  (\tag -> do
                     guard $ tag `notElem` ["AA", "ZZ"]
                     let [(c', _)] = filter ((/= coord) . fst) (pmPortals M.! tag)
                     pure (c', Just (tag, if isOnInnerRim coord then PsInner else PsOuter)))
                  lsPre

      miGraphPre :: M.Map Coord [Either String Coord]
      miGraphPre = pmGraph

debugMapInfo :: MapInfo -> IO ()
debugMapInfo
  MapInfo
    { miGraph
    , miStartEnd = (startCoord, endCoord)
    , miDist
    , miInnerOuter = (inner, outer)
    } = do
    let isOuter = inRange ((minR, minC), (maxR, maxC))
        MinMax2D ((minR, maxR), (minC, maxC)) = outer
        isInner = inRange ((a, c), (b, d))
          where
            MinMax2D ((a, b), (c, d)) = inner

    forM_ [minR - 1 .. maxR + 1] $ \r -> do
      let render c =
            if
                | isInner coord -> ' '
                | coord == startCoord -> 'S'
                | coord == endCoord -> 'E'
                | isOuter coord ->
                  case miGraph M.!? coord of
                    Nothing -> '█'
                    Just _ -> '.'
                | otherwise -> ' '
            where
              coord = (r, c)
      putStrLn (fmap render [minC - 2 .. maxC + 2])
    let showRoutes = True
    when showRoutes do
      c <- forM (M.toAscList miDist) $ \(MinMax (c0, c1), dist) -> do
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
          performEnqs <- forM (M.keys (miGraph M.! u)) $ \v -> do
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
  let deg = M.size cs
  guard $ deg <= 2
  pure (coord PQ.:-> deg)

-- TODO: we should probably have an util module that deal with this kind of things for undirected graphs.
getDist :: M.Map (MinMax Coord) Int -> (Coord, Coord) -> Maybe Int
getDist m p = m M.!? minMaxFromPair p

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
            let [(c', _)] = M.toList (miGraph M.! c)
                miGraph' = M.adjust (M.delete c) c' $ M.delete c miGraph
                q2 = PQ.insert c' (M.size $ miGraph' M.! c') q1
             in simplifyMapInfoAux mi {miGraph = miGraph'} q2
          2 ->
            let [(c1, p1), (c2, p2)] = M.toList (miGraph M.! c)
                mOldDist = getDist miDist (c1, c2)
                newDist = fromJust (getDist miDist (c, c1)) + fromJust (getDist miDist (c, c2))
                safeToPrune = case (p1, p2) of
                  (Nothing, Nothing) ->
                    case mOldDist of
                      Nothing -> True
                      Just oldDist -> newDist <= oldDist
                  _ -> False
                miGraph' =
                  M.adjust (M.insert c1 Nothing . M.delete c) c2 $
                    M.adjust (M.insert c2 Nothing . M.delete c) c1 $
                      M.delete c miGraph
                miDist' =
                  -- probably not worth removing old ones
                  M.insert (minMaxFromPair (c1, c2)) newDist miDist
                enqueue cx = PQ.insert cx (M.size $ miGraph' M.! cx)
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
        parsed = parseMap rawFloor
        mi = mkMapInfo parsed
        mi' = simplifyMapInfo mi
        (Just endDist, _) = runSpfa mi'
        debug = False
    when debug do
      debugMapInfo mi'
    answerShow endDist
