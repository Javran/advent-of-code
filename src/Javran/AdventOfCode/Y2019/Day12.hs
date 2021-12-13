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
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2019.Day12
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
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Debug.Trace
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Linear.Affine
import Linear.V3
import Linear.Vector
import Text.ParserCombinators.ReadP hiding (count, many)
import Turtle (system)

data Day12 deriving (Generic)

moonLocationP :: ReadP (Point V3 Int)
moonLocationP = do
  let intP = readS_to_P (reads @Int)
  _ <- string "<x="
  x <- intP
  _ <- string ", y="
  y <- intP
  _ <- string ", z="
  z <- intP
  _ <- string ">"
  pure $ P (V3 x y z)

type Moon = (Point V3 Int {- pos -}, V3 Int {- vel -})

type System = [Moon]

moonEnerge :: Moon -> Int
moonEnerge (P (V3 x y z), V3 vx vy vz) = sum (fmap abs [x, y, z]) * sum (fmap abs [vx, vy, vz])

systemEnerge :: System -> Int
systemEnerge = sum . fmap moonEnerge

stepOneDim :: [(Int, Int)] -> [(Int, Int)]
stepOneDim xs0 = zip locs' vels'
  where
    (locs, vels) = unzip xs0
    vels' = zipWith (+) vels gravity
    locs' = zipWith (+) locs vels'
    gravity :: [Int]
    gravity = appEndo (foldMap Endo gravityMods) $ replicate (length xs0) 0
      where
        gravityMods = do
          -- pairwise context, indexed by i and j.
          ((i :: Int, (locA, _)), xs1) <- pickInOrder (zip [0 ..] xs0)
          ((j, (locB, _)), _) <- pickInOrder xs1
          pure $ case compare locA locB of
            LT -> (& element i %~ succ) . (& element j %~ pred)
            EQ -> id -- do nothing
            GT -> (& element i %~ pred) . (& element j %~ succ)

stepSystem :: System -> System
stepSystem moons = zipWith3 combine xs' ys' zs'
  where
    combine (px, vx) (py, vy) (pz, vz) =
      (P (V3 px py pz), (V3 vx vy vz))

    xs = fmap (\(p, v) -> (p ^. _x, v ^. _x)) moons
    ys = fmap (\(p, v) -> (p ^. _y, v ^. _y)) moons
    zs = fmap (\(p, v) -> (p ^. _z, v ^. _z)) moons

    xs' = stepOneDim xs
    ys' = stepOneDim ys
    zs' = stepOneDim zs

instance Solution Day12 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (extraOps, rawInput) <- consumeExtraLeadingLines <$> getInputS
    let moons = fmap (consumeOrDie moonLocationP) . lines $ rawInput
        initSys :: System
        initSys = zip moons (repeat 0)
        steps = case extraOps of
          Just xs -> read (head xs)
          Nothing -> 1000
    answerShow $ systemEnerge (iterate stepSystem initSys !! steps)
