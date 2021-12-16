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

data Dir = U | D | L | R deriving (Show)

data MapInfo = MapInfo
  { miScaffolds :: S.Set Coord
  , miRobot :: (Dir, Coord)
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

{-
  TODO:

  for now I have no idea how to break things down,
  but it seems safe to assume a "principle path" that we can compute by:

  - keep moving forward on scaffolds until it's not possible
  - turn left or right (should be exactly one available)

  Once we have the full principle path constructed,
  we can see what should we do about it.

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
            MapInfo {miScaffolds} = parseRawMap rawMap
            intersections = do
              coord <- S.toList miScaffolds
              guard $ all (`S.member` miScaffolds) (udlrOfCoord coord)
              pure coord
        answerShow (sum $ fmap (uncurry (*)) intersections)
        putStrLn rawMap
      Just _ ->
        -- presence of extra field means that the test content is the map.
        print (parseRawMap rawInput)
