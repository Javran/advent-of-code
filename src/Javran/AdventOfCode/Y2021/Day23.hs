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

module Javran.AdventOfCode.Y2021.Day23
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Lens
import Control.Monad
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
import System.IO.Unsafe
import Text.ParserCombinators.ReadP hiding (count, many)

data Day23 deriving (Generic)

data AmpType = A | B | C | D deriving (Eq, Ord, Enum, Show)

{-
Ax20
Bx12
Cx11
Dx11
 #############
 #...........#
 ###A#B#C#D###
   #A#B#C#D#
   #########
 -}
type Coord = (Int, Int)

can'tStops :: S.Set Coord
can'tStops = S.fromList [(1, 3), (1, 5), (1, 7), (1, 9)]

moveTargets :: AmpType -> S.Set Coord
moveTargets = \case
  A -> S.fromList $ fmap (,3) rs
  B -> S.fromList $ fmap (,5) rs
  C -> S.fromList $ fmap (,7) rs
  D -> S.fromList $ fmap (,9) rs
  where
    rs = [2, 3, 4, 5]

theMap :: M.Map Coord (S.Set Coord)
theMap =
  M.fromList $
    (fmap . second)
      S.fromList
      [ ((1, 1), [(1, 2)])
      , ((1, 2), [(1, 1), (1, 3)])
      , ((1, 3), [(1, 2), (1, 4), (2, 3)])
      , ((1, 4), [(1, 3), (1, 5)])
      , ((1, 5), [(1, 4), (1, 6), (2, 5)])
      , ((1, 6), [(1, 5), (1, 7)])
      , ((1, 7), [(1, 6), (1, 8), (2, 7)])
      , ((1, 8), [(1, 7), (1, 9)])
      , ((1, 9), [(1, 8), (1, 10), (2, 9)])
      , ((1, 10), [(1, 9), (1, 11)])
      , ((1, 11), [(1, 10)])
      , ((2, 3), [(1, 3), (3, 3)])
      , ((2, 5), [(1, 5), (3, 5)])
      , ((2, 7), [(1, 7), (3, 7)])
      , ((2, 9), [(1, 9), (3, 9)])
      , ((3, 3), [(2, 3), (4, 3)])
      , ((3, 5), [(2, 5), (4, 5)])
      , ((3, 7), [(2, 7), (4, 7)])
      , ((3, 9), [(2, 9), (4, 9)])
      , ((4, 3), [(3, 3), (5, 3)])
      , ((4, 5), [(3, 5), (5, 5)])
      , ((4, 7), [(3, 7), (5, 7)])
      , ((4, 9), [(3, 9), (5, 9)])
      , ((5, 3), [(4, 3)])
      , ((5, 5), [(4, 5)])
      , ((5, 7), [(4, 7)])
      , ((5, 9), [(4, 9)])
      ]

type WorldState = [S.Set Coord]

pprWorldState :: WorldState -> IO ()
pprWorldState ws = do
  let ampLocs :: M.Map Coord AmpType
      ampLocs = M.fromList do
        (aTy, cs) <- zip [A .. D] ws
        coord <- S.toList cs
        pure (coord, aTy)
      raw =
        [ "#############"
        , "#..!.!.!.!..#"
        , "###.#.#.#.###"
        , "  #.#.#.#.#  "
        , "  #.#.#.#.#  "
        , "  #.#.#.#.#  "
        , "  #########  "
        ]
  forM_ [0 .. length raw -1] $ \r -> do
    let render c
          | Just ampTy <- ampLocs M.!? (r, c) =
            show ampTy
          | otherwise = [raw !! r !! c]
    putStrLn $ concatMap render [0 .. length (head raw) -1]

targetWorld = fmap moveTargets [A .. D]

homingPriority :: WorldState -> Int
homingPriority ws = - (sum $ fmap S.size $ zipWith S.intersection targetWorld ws)

findNextMoves :: AmpType -> Coord -> WorldState -> [(Coord, WorldState, Int)]
findNextMoves ampType initCoord wsPre = findNextMovesAux (PQ.singleton initCoord 0) (S.singleton initCoord)
  where
    ampLocs :: M.Map Coord AmpType
    ampLocs = M.fromList do
      (aTy, cs) <- zip [A .. D] ws
      coord <- S.toList cs
      pure (coord, aTy)
    moveCost = case ampType of
      A -> 1
      B -> 10
      C -> 100
      D -> 1000
    ws = wsPre & ix (fromEnum ampType) %~ S.delete initCoord
    blockings :: S.Set Coord
    blockings = S.unions ws
    findNextMovesAux q0 discovered = case PQ.minView q0 of
      Nothing -> []
      Just ((coord) PQ.:-> energy, q1) ->
        [ (coord, ws & ix (fromEnum ampType) %~ S.insert coord, energy)
        | S.notMember coord can'tStops
        ]
          <> let (r, c) = coord
                 isInHallway = r == 1
                 nexts = do
                   Just coords' <- [theMap M.!? coord]
                   coord'@(r', _) <- S.toList coords'
                   when (isInHallway && 2 <= r' && r' <= 5) do
                     let myMoveTargets = moveTargets ampType
                     guard $ S.member coord' myMoveTargets
                     -- also room must be clear
                     forM_ myMoveTargets \tgtCoord ->
                       guard $ case ampLocs M.!? tgtCoord of
                         Nothing -> True
                         Just ampType' -> ampType' == ampType
                   guard $ S.notMember coord' blockings
                   guard $ S.notMember coord' discovered
                   pure coord'
                 q2 = foldr upd q1 nexts
                   where
                     upd coord' =
                       let energy' = energy + moveCost
                        in PQ.alter
                             (\case
                                Nothing -> Just energy'
                                Just e -> Just (min e energy'))
                             coord'
              in findNextMovesAux q2 (S.union discovered (S.fromList nexts))

stepBfs = False

bfs q0 discovered = case PQ.minView q0 of
  Nothing -> error "queue exhausted"
  Just (ws PQ.:-> (_hp, energy), q1) ->
    if ws == targetWorld
      then energy
      else
        let nexts = do
              (ampType, coords) <- zip [A .. D] ws
              coord@(r, c) <- S.toList coords
              let isInHallway = r == 1
                  curMoveTargets = moveTargets ampType
              (coord', ws', incr) <- findNextMoves ampType coord ws
              -- if in hallway, must move to a room
              when isInHallway $
                guard $ S.member coord' curMoveTargets
              guard $ S.notMember ws' discovered
              pure (ws', incr)
            q2 = foldr upd q1 nexts
              where
                upd (ws', incr) curQ =
                  PQ.alter
                    (\case
                       Nothing -> Just (hp, energy')
                       Just (_, e) -> Just (hp, min e energy'))
                    ws'
                    curQ
                  where
                    hp = homingPriority ws'
                    energy' = energy + incr
         in if stepBfs
              then unsafePerformIO do
                putStrLn "cur"
                pprWorldState ws
                print (homingPriority ws, energy)
                let
                putStrLn "expansion:"
                forM_ nexts \(ws', e) -> do
                  pprWorldState ws'
                  print (homingPriority ws', e)
                putStrLn "==="
                -- readLn :: IO Int
                pure $ bfs q2 (S.union discovered (S.fromList $ fmap fst nexts))
              else bfs q2 (S.union discovered (S.fromList $ fmap fst nexts))

instance Solution Day23 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- lines <$> getInputS
    let theRoom = do
          (r, rs) <- zip [0 ..] xs
          (c, x) <- zip [0 ..] rs
          guard $ x `notElem` "# "
          pure ((r, c), x)
        theRoom' = M.fromListWith (<>) do
          (coord, _) <- theRoom
          coord' <- udlrOfCoord coord
          guard $ coord' `elem` fmap fst theRoom
          pure (coord, S.singleton coord')
        startState =
          [ -- A
            S.fromList [(2, 3), (3, 9), (4, 7), (5, 7)]
          , -- B
            S.fromList [(2, 7), (3, 7), (4, 5), (5, 3)]
          , -- C
            S.fromList [(3, 5), (4, 9), (5, 5), (5, 9)]
          , -- D
            S.fromList [(2, 5), (2, 9), (3, 3), (4, 3)]
          ]
    pprWorldState startState
    pprWorldState targetWorld
    print $ bfs (PQ.singleton startState (homingPriority startState, 0)) (S.singleton startState)
