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

module Javran.AdventOfCode.Y2019.Day15
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
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
import Data.Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2019.IntCode

data Day15 deriving (Generic)

type Coord = (Int, Int) -- row and col

data Cell = CEmpty | CWall deriving (Show, Eq)

data Dir = N | S | W | E deriving (Show, Bounded, Enum)

allDirs :: [Dir]
allDirs = universe

applyDir :: Dir -> Coord -> Coord
applyDir = \case
  N -> first pred
  S -> first succ
  W -> second pred
  E -> second succ

{-
  TODO: assumptions:

  - floor does not change as droid moves.
  - space is close, we won't eventually stuck into exploring
    unbounded spaces.
  - the space is reasonably small that pathfinding simply with BFS
    is good enough for this puzzle.

 -}

findPathToAnyTarget
  :: M.Map Coord Cell -> S.Set Coord -> Seq.Seq (Coord, [Dir]) -> S.Set Coord -> Maybe (Coord, [Dir])
findPathToAnyTarget floorInfo unknowns q discovered = case q of
  Seq.Empty -> Nothing
  (coord, revPath) Seq.:<| q' ->
    if S.member coord unknowns
      then Just (coord, revPath)
      else
        let nextCoords = do
              d <- allDirs
              let coord' = applyDir d coord
              guard $
                S.member coord' unknowns
                  || M.lookup coord' floorInfo == Just CEmpty
              guard $ S.notMember coord' discovered
              pure (coord', d : revPath)
         in findPathToAnyTarget
              floorInfo
              unknowns
              (q' <> Seq.fromList nextCoords)
              (S.union discovered (S.fromList $ fmap fst nextCoords))

data SystemState = SystemState
  { ssFloorInfo :: M.Map Coord Cell -- known part of the floor
  , ssDroid :: Coord -- droid location
  , ssUnknowns :: S.Set Coord -- unknown coords adjacent to known floor cells.
  , ssProg :: IO (Result ()) -- suspected program
  , ssOxygenSys :: Maybe Coord
  }

explore :: StateT SystemState IO ()
explore = do
  {-
    Step 1: path find to the closest unknown, compute path xs
      (terminate if the set is empty)
    Step 2: execute (init xs), expect success.
    Step 3: execute (last xs) and find out whether it's a wall
    Step 4: update unknown appropriately.
   -}
  SystemState {ssFloorInfo, ssDroid, ssUnknowns} <- get
  let mExploreTarget =
        findPathToAnyTarget
          ssFloorInfo
          ssUnknowns
          (Seq.singleton (ssDroid, []))
          (S.singleton ssDroid)
  case mExploreTarget of
    Nothing -> pure ()
    Just (_, []) -> unreachable
    Just (_targetCoord, (lastStep : safeStepsRev)) -> do
      -- execute safe steps
      forM_ (reverse safeStepsRev) $ \d -> do
        True <- execCommand d
        pure ()
      _ <- execCommand lastStep
      explore
  where
    updateInfo coord = \case
      CWall ->
        modify
          (\ss ->
             ss
               { ssUnknowns = S.delete coord (ssUnknowns ss)
               , ssFloorInfo = M.insert coord CWall (ssFloorInfo ss)
               })
      CEmpty -> do
        SystemState {ssUnknowns, ssFloorInfo} <- get
        let ssFloorInfo' = M.insert coord CEmpty ssFloorInfo
            expandCoords = S.fromList do
              c' <- udlrOfCoord coord
              guard $ M.notMember c' ssFloorInfo
              pure c'
            ssUnknowns' = S.union expandCoords $ S.delete coord ssUnknowns
        modify (\ss -> ss {ssUnknowns = ssUnknowns', ssFloorInfo = ssFloorInfo'})

    execCommand dir = do
      let cmd = case dir of
            N -> 1
            S -> 2
            W -> 3
            E -> 4
      SystemState {ssProg, ssDroid} <- get
      let droidCoord' = applyDir dir ssDroid
      ([reply], prog') <- liftIO $ communicate [cmd] 1 ssProg
      modify (\ss -> ss {ssProg = prog'})
      case reply of
        0 -> do
          -- hitting a wall.
          updateInfo droidCoord' CWall
          pure False
        1 -> do
          -- empty
          modify (\ss -> ss {ssDroid = droidCoord'})
          updateInfo droidCoord' CEmpty
          pure True
        2 -> do
          modify (\ss -> ss {ssDroid = droidCoord'})
          -- also oxygen system located
          modify (\ss -> ss {ssOxygenSys = Just droidCoord'})
          updateInfo droidCoord' CEmpty
          pure True
        _ -> error $ "invalid reply: " <> show reply

bfsForOxygen floorInfo acc discovered = \case
  Seq.Empty -> acc
  ((coord, depth) Seq.:<| q) ->
    let nextCoords = do
          c' <- udlrOfCoord coord
          Just CEmpty <- pure (floorInfo M.!? c')
          guard $ S.notMember c' discovered
          pure (c', depth + 1)
     in bfsForOxygen
          floorInfo
          (M.insert coord depth acc)
          (S.union discovered $ S.fromList (fmap fst nextCoords))
          (q <> Seq.fromList nextCoords)

instance Solution Day15 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- parseCodeOrDie <$> getInputS
    let initSys =
          SystemState
            { ssFloorInfo = M.singleton (0, 0) CEmpty
            , ssDroid = (0, 0)
            , ssUnknowns = S.fromList (udlrOfCoord (0, 0))
            , ssProg = void <$> startProgramFromFoldable xs
            , ssOxygenSys = Nothing
            }
    ((), SystemState {ssFloorInfo, ssOxygenSys= Just locOxy}) <- runStateT explore initSys
    let Just ((Min minR, Max maxR), (Min minC, Max maxC)) =
          foldMap (\(r, c) -> Just ((Min r, Max r), (Min c, Max c))) $ M.keys ssFloorInfo
        display = False
    when display $
      forM_ [minR .. maxR] $ \r -> do
        let render c =
              if
                  | (r, c) == (0, 0) -> "><"
                  |  (r, c) == locOxy -> "OX"
                  | otherwise -> case ssFloorInfo M.!? (r, c) of
                    Nothing -> "??"
                    Just CWall -> "##"
                    Just CEmpty -> "  "
            curRow = fmap render [minC .. maxC]
        putStrLn (concat curRow)
    let Just (_, revPath) =
          findPathToAnyTarget
            ssFloorInfo
            (S.singleton locOxy)
            (Seq.singleton ((0, 0), []))
            (S.singleton (0, 0))
    answerShow (length revPath)
    answerShow (maximum $ M.elems (bfsForOxygen ssFloorInfo M.empty (S.singleton locOxy) (Seq.singleton (locOxy,0))))
