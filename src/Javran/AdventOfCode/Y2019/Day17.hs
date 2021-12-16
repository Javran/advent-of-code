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

module Javran.AdventOfCode.Y2019.Day17
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Control.Monad.Writer.CPS
import Data.Bifunctor
import Data.Bits
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
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2019.IntCode
import Numeric
import Text.ParserCombinators.ReadP hiding (count, many)

data Day17 deriving (Generic)

type Coord = (Int, Int)

-- alters rotate counter-clockwise
data Dir
  = U
  | L
  | D
  | R
  deriving (Show, Enum, Bounded)

turnLeft :: Dir -> Dir
turnLeft d = cycle universe !! (fromEnum d + 1)

turnRight :: Dir -> Dir
turnRight d = cycle universe !! (fromEnum d + 3)

applyDir :: Dir -> Coord -> Coord
applyDir = \case
  U -> first pred
  D -> first succ
  L -> second pred
  R -> second succ

type Robot = (Dir, Coord)

data MapInfo = MapInfo
  { miScaffolds :: S.Set Coord
  , miRobot :: Robot
  }
  deriving (Show)

parseRawMap :: String -> MapInfo
parseRawMap rawMap = MapInfo {miScaffolds, miRobot}
  where
    (miScaffolds, Data.Monoid.Last (Just miRobot)) = mconcat do
      (r, row) <- zip [0 ..] (lines rawMap)
      (c, x) <- zip [0 ..] row
      -- We assume that map always starts with robot on scaffolds.
      guard $ x `notElem` ".X"
      let coord = (r, c)
      let mRobot = case x of
            '^' -> pure (U, coord)
            'v' -> pure (D, coord)
            '<' -> pure (L, coord)
            '>' -> pure (R, coord)
            _ -> Nothing
      pure (S.singleton coord, Data.Monoid.Last mRobot)

data Move
  = Forward Int
  | TurnLeft
  | TurnRight

instance Show Move where
  show = \case
    Forward v -> show v
    TurnLeft -> "L"
    TurnRight -> "R"

nextMoves :: MapInfo -> Robot -> WriterT [Move] Maybe (Robot)
nextMoves MapInfo {miScaffolds} (dir, coord) = do
  let frontCoord = applyDir dir coord
      leftCoord = applyDir (turnLeft dir) coord
      rightCoord = applyDir (turnRight dir) coord
  {-
    If can't move forward, try left or right, `Nothing` if neither is available.
   -}
  dirMod <-
    if
        | S.member frontCoord miScaffolds ->
          pure id
        | S.member leftCoord miScaffolds -> do
          tell [TurnLeft]
          pure turnLeft
        | S.member rightCoord miScaffolds -> do
          tell [TurnRight]
          pure turnRight
        | otherwise -> lift Nothing
  let dir' = dirMod dir
      frontCoords = takeWhile (`S.member` miScaffolds) $ tail $ iterate (applyDir dir') coord
  tell [Forward (length frontCoords)]
  pure (dir', last frontCoords)

computeMoves :: MapInfo -> Robot -> [Move]
computeMoves mi miRobot =
  concat $
    unfoldr
      (\robot -> do
         (robot', w) <- runWriterT (nextMoves mi robot)
         pure (w, robot'))
      miRobot

{-
  TODO:

  for now I have no idea how to break things down,
  but it seems safe to assume a "principle path" that we can compute by:

  - keep moving forward on scaffolds until it's not possible
  - turn left or right (should be exactly one available)

  Once we have the full principle path constructed,
  we can see what should we do about it.

 -}

{-
my login:

   123456789X123456789X
A= L,12,L,10,R,8,L,12,
B= R,8,R,10,R,12,
A= L,12,L,10,R,8,L,12,
B= R,8,R,10,R,12,
C= L,10,R,12,R,8,
C= L,10,R,12,R,8,
B= R,8,R,10,R,12,
A= L,12,L,10,R,8,L,12,
B= R,8,R,10,R,12,
C= L,10,R,12,R,8

m= A,B,A,B,C,C,B,A,B,C

example input:

   123456789X123456789X
A= R,6,L,10,R,10,R,10,
B= L,10,L,12,R,10,
A= R,6,L,10,R,10,R,10,
A= L,10,L,12,R,10,
A= R,6,L,10,R,10,R,10,
C= R,6,L,12,L,10,
A= R,6,L,10,R,10,R,10,
C= R,6,L,12,L,10,
B= L,10,L,12,R,10,
C= R,6,L,12,L,10

m= A,B,A,A,A,C,A,C,B,C

 -}

instance Solution Day17 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (extraOps, rawInput) <- consumeExtraLeadingLines <$> getInputS
    case extraOps of
      Nothing -> do
        let xs = parseCodeOrDie rawInput
        (_, out) <- runProgram xs []
        let rawMap = fmap chr out
            mi@MapInfo {miScaffolds, miRobot} = parseRawMap rawMap
            intersections = do
              coord <- S.toList miScaffolds
              guard $ all (`S.member` miScaffolds) (udlrOfCoord coord)
              pure coord
        answerShow (sum $ fmap (uncurry (*)) intersections)
        print (computeMoves mi miRobot)
        putStrLn rawMap
      Just _ -> do
        let mi@MapInfo {miScaffolds, miRobot} = parseRawMap rawInput
            moves = computeMoves mi miRobot
        print miScaffolds
        print miRobot
        print moves
