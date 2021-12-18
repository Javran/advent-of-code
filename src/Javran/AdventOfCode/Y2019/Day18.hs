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

module Javran.AdventOfCode.Y2019.Day18
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import qualified Data.Array as Arr
import qualified Data.Array.IArray as IArr
import Data.Bifunctor
import Data.Bool
import Data.Char
import Data.Either
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
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, many)

data Day18 deriving (Generic)

{-
  Notes:

  Looks like all those maps have some properties that we can take advantage of:

  - there very little presence of 2x2 open cells in the entire map, meaning that there is probably
    only handful of ways (I'll bet 1 or 2 without even looking into details)
    to go from one place to another.

  - if we only look at empty cells,
    there are many tunnels (degree of 2) and dead ends (degree of 1), that
    we can probably get rid of while keeping necessary info of the map intact.

  So instead of performing some search on the input map, we can condense it
  into a graph whose nodes are map cells and edges encode distances between nodes.

  Let's just first do this simplification step and see how many things are left.

 -}

data Cell
  = COpen
  | CEntrance
  | CWall
  | CKey Int -- [0..25] for letters
  | CDoor Int -- [0..25] for letters
  deriving (Eq)

instance Show Cell where
  show =
    (: []) . \case
      COpen -> '.'
      CEntrance -> '@'
      CWall -> '#'
      CKey v -> chr (v + ord 'a')
      CDoor v -> chr (v + ord 'A')

parseCell :: Char -> Cell
parseCell = \case
  '.' -> COpen
  '@' -> CEntrance
  '#' -> CWall
  ch ->
    if
        | isAsciiLower ch -> CKey (ord ch - ord 'a')
        | isAsciiUpper ch -> CDoor (ord ch - ord 'A')
        | otherwise -> errInvalid

type Coord = (Int, Int) -- row and col

data MapInfo = MapInfo
  { miGet :: Coord -> Maybe Cell
  , miGraph :: M.Map Coord (S.Set Coord)
  , -- | distance between two coords (c,c'), where c < c'
    -- since the graph is bidirectional, we only need to store half.
    miDist :: M.Map (Coord, Coord) Int
  , miAllKeys :: IS.IntSet
  }

getDist :: M.Map (Coord, Coord) Int -> (Coord, Coord) -> Maybe Int
getDist m (a, b) = m M.!? if a < b then (a, b) else (b, a)

safeIndexArr :: (Arr.Ix i, IArr.IArray a e) => a i e -> i -> Maybe e
safeIndexArr arr i =
  arr IArr.! i
    <$ guard (inRange (IArr.bounds arr) i)

mkMapInfo :: IArr.Array (Int, Int) Cell -> MapInfo
mkMapInfo floorPlan = MapInfo {miGraph, miDist, miGet = safeIndexArr floorPlan, miAllKeys}
  where
    miGraph = M.unionsWith (<>) gPre
    miDist = M.unionsWith (+) dPre
    miAllKeys = IS.fromList do
      CKey k <- IArr.elems floorPlan
      pure k
    (gPre, dPre) = unzip do
      (c, x) <- Arr.assocs floorPlan
      guard $ x /= CWall
      c' <- udlrOfCoord c
      Just x' <- pure $ safeIndexArr floorPlan c'
      guard $ x' /= CWall
      pure
        ( M.singleton c (S.singleton c')
        , if c < c' then M.singleton (c, c') (1 :: Int) else M.empty
        )

simplifyMapInfo :: MapInfo -> MapInfo
simplifyMapInfo mi@MapInfo {miGraph, miGet} = simplifyMapInfoAux mi $ PQ.fromList do
  coord <- M.keys miGraph
  Just COpen <- pure (miGet coord)
  pure (coord PQ.:-> S.size (miGraph M.! coord))

{-
  For the priority queue, the invariant is that only COpen cells are allowed to be in the queue.
 -}
simplifyMapInfoAux :: MapInfo -> PQ.PSQ Coord Int -> MapInfo
simplifyMapInfoAux mi@MapInfo {miGraph, miGet, miDist} q0 = case PQ.minView q0 of
  Nothing -> mi
  Just (c PQ.:-> deg, q1) -> case deg of
    1 ->
      let [c'] = S.toList (miGraph M.! c)
          miGraph' = M.adjust (S.delete c) c' $ M.delete c miGraph
          q2 =
            if miGet c' == Just COpen
              then PQ.insert c' (S.size $ miGraph' M.! c') q1
              else q1
       in simplifyMapInfoAux mi {miGraph = miGraph'} q2
    2 ->
      let [c1, c2] = S.toList (miGraph M.! c)
          miGraph' =
            {-
              note that in this process we could create a node that links to itself,
              if the original graph contains one.
              but that will get eliminated in a subsequent simplification (by degree 1 case).
             -}
            M.adjust (S.insert c1 . S.delete c) c2 $
              M.adjust (S.insert c2 . S.delete c) c1 $
                M.delete c miGraph
          miDist' =
            let p = if c1 < c2 then (c1, c2) else (c2, c1)
                newDist = fromJust (getDist miDist (c, c1)) + fromJust (getDist miDist (c, c2))
             in -- probably not worth removing old ones
                M.insert p newDist miDist
          enqueue cx =
            if miGet cx == Just COpen
              then PQ.insert cx (S.size $ miGraph' M.! cx)
              else id
          q2 = enqueue c1 $ enqueue c2 $ q1
       in simplifyMapInfoAux mi {miGraph = miGraph', miDist = miDist'} q2
    _
      | deg >= 3 ->
        -- meaning all deg 1 and 2 are done.
        mi
    _ -> unreachable

type MissingKeys = IS.IntSet

type SearchQueue =
  PQ.PSQ
    (Coord, MissingKeys)
    ( Int -- # of steps taken
    , Int -- size of missing keys.
    )

bfs (mi@MapInfo {miGraph, miGet, miDist}) (q0 :: SearchQueue) discovered = case PQ.minView q0 of
  Nothing -> error "queue exhausted"
  Just ((coord, missingKeys) PQ.:-> (stepCount, missingKeyCount), q1) ->
    if missingKeyCount == 0
      then stepCount
      else
        let nexts = do
              coord' <- S.toList (miGraph M.! coord)
              let stepCount' = stepCount + fromJust (getDist miDist (coord, coord'))
              missingKeys' <-
                let ok = pure missingKeys
                 in case fromJust (miGet coord') of
                      COpen -> ok
                      CEntrance -> ok
                      CWall -> unreachable
                      CKey k -> pure (IS.delete k missingKeys)
                      CDoor k -> guard (IS.notMember k missingKeys) *> ok
              guard $ S.notMember (coord', missingKeys') discovered
              pure ((coord', missingKeys'), stepCount')
            discovered' = foldl' (\acc (x, _) -> S.insert x acc) discovered nexts
            q2 = foldl' updateQ q1 nexts
              where
                updateQ curQ (k@(_, missingKeys'), stepCount') =
                  PQ.alter
                    (let p' = (stepCount', IS.size missingKeys')
                      in \case
                           Nothing -> Just p'
                           Just p -> Just (min p' p))
                    k
                    curQ
         in bfs mi q2 discovered'

startBfs mi@MapInfo {miGet, miGraph, miAllKeys} =
  bfs
    mi
    (PQ.singleton startKey (0, IS.size miAllKeys))
    (S.singleton startKey)
  where
    startKey = (coordEnt, miAllKeys)
    coordEnt : _ = do
      coord <- M.keys miGraph
      Just CEntrance <- pure (miGet coord)
      pure coord

instance Solution Day18 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- lines <$> getInputS
    let rows = length xs
        cols = length (head xs)
        floorPlan = Arr.array
          ((0, 0), (rows -1, cols -1))
          do
            (r, rs) <- zip [0 ..] xs
            (c, x) <- zip [0 ..] rs
            pure ((r, c), parseCell x)
        mi = simplifyMapInfo $ mkMapInfo floorPlan
    forM_ [0 .. rows -1] $ \r -> do
      let render c = case miGet mi coord of
            Nothing -> unreachable
            Just cell -> case cell of
              COpen -> if isJust v then show $ fromJust (miGet mi coord) else " "
              _ -> show $ fromJust (miGet mi coord)
            where
              v = miGraph mi M.!? coord
              coord = (r, c)
      putStrLn (concatMap render [0 .. cols -1])
    c <- forM (M.toAscList (miDist mi)) $ \((c0, c1), dist) -> do
      let v0 = fromJust (miGet mi c0)
          v1 = fromJust (miGet mi c1)
      if (M.member c0 (miGraph mi) && M.member c1 (miGraph mi))
        then do
          putStrLn $ show c0 <> " '" <> show v0 <> "' <=> " <> show c1 <> " '" <> show v1 <> "': " <> show dist
          pure (1 :: Int)
        else pure 0
    print $ sum c
    print $ startBfs mi
