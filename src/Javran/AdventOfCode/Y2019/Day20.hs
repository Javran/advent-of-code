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
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day20 deriving (Generic)

type Coord = (Int, Int)

data MapInfo = MapInfo
  { miGraph :: M.Map Coord (S.Set Coord)
  , miStartEnd :: (Coord, Coord)
  }

mkMapInfo :: Arr.Array Coord Char -> MapInfo
mkMapInfo rawFloor =
  MapInfo
    { miStartEnd = (startCoord, endCoord)
    , miGraph
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
debugMapInfo MapInfo {miGraph, miStartEnd = (startCoord, endCoord)} = do
  let Just (MinMax2D ((minR, maxR), (minC, maxC))) =
        foldMap (Just . minMax2D) $ M.keys miGraph
  forM_ [minR - 1 .. maxR + 1] $ \r -> do
    let render c =
          if
              | coord == startCoord -> 'S'
              | coord == endCoord -> 'E'
              | minR <= r && r <= maxR && minC <= c && c <= maxC ->
                case miGraph M.!? coord of
                  Nothing -> '#'
                  Just cs ->
                    if any (`notElem` (udlrOfCoord coord)) cs then '~' else '.'
              | otherwise -> ' '
          where
            coord = (r, c)
    putStrLn (fmap render [minC - 2 .. maxC + 2])

shortestPath :: MapInfo -> State (M.Map Coord Int) (Maybe Int)
shortestPath MapInfo {miStartEnd = (startCoord, endCoord), miGraph} = do
  put (M.singleton startCoord 0)
  fix
    (\loop ->
       \case
         Seq.Empty ->
           gets (M.!? endCoord)
         (u Seq.:<| q0) -> do
           distU <- gets (M.! u)
           performEnqs <- forM (S.toList (miGraph M.! u)) $ \v -> do
             let distV' = distU + 1
             mDistV <- gets (M.!? v)
             if maybe True (distV' <) mDistV
               then do
                 modify (M.insert v distV')
                 pure (Seq.|> v)
               else pure id
           loop $ appEndo (foldMap Endo performEnqs) q0)
    (Seq.singleton startCoord)

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
    -- debugMapInfo mi
    answerShow (fromJust $ evalState (shortestPath mi) M.empty)
