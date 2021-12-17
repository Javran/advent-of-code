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

module Javran.AdventOfCode.Y2021.Day17
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
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
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Linear.Affine
import Linear.V2
import Text.ParserCombinators.ReadP hiding (count, many)

data Day17 deriving (Generic)

rangeP :: ReadP ((Int, Int), (Int, Int))
rangeP = do
  let intP = readS_to_P @Int reads
  _ <- string "target area: x="
  xMin <- intP
  _ <- string ".."
  xMax <- intP
  _ <- string ", y="
  yMin <- intP
  _ <- string ".."
  yMax <- intP
  pure ((xMin, xMax), (yMin, yMax))

step :: (Point V2 Int, V2 Int) -> (Point V2 Int, V2 Int)
step (pos, vel@(V2 vx vy)) = (pos .+^ vel, V2 vx' (vy -1))
  where
    vx' = case compare vx 0 of
      LT -> vx + 1
      EQ -> 0
      GT -> vx -1

simulateTill :: Int -> V2 Int -> [Point V2 Int]
simulateTill xMax initVel =
  fmap fst $
    takeWhile (\(P (V2 x y), _) -> x <= xMax && y > -2000) $
      iterate step (0, initVel)

{-

  Let's see how y-location changes over time:

  t  loc       vel
  0  0         vy
  1  vy        vy-1
  2  2vy-1     vy-2
  3  3vy-1-2   vy-3
  4  3vy-1-2-3 vy-4

  where vy is the initial y-velocity.

  from this we can derive that:

  y(t) = vy * t - (t-1) * t / 2
       = (2 * vy - t + 1) * t / 2

  peak y is reached (if we shot upwards) when t = vy or vy+1
  (both are the same, since velocity at t=vy is 0, y will stay unchanged at t=vy+1)

  max(y) (if shot upwards)
  = (2 * vy - t + 1) * t / 2
  = (2 * vy - vy + 1) * vy / 2 (t = vy)
  = (vy + 1) * vy / 2

  Now, go back to the equation:

  y(t) = (2 * vy - t + 1) * t / 2

  This makes it obvious that the solution to y(t) = 0 is:

  - t = 0
  - t = 2 * vy + 1

  We are interested in the second case, at that time,
  the y-velocity is:

  vy - t = -vy-1

  Looking at sample input and my login input, it seems that the assumption
  is that target range always have y < 0. So our plan is to shot upwards
  as far as we can, and let it go down to y < 0 target region.

  When landing on y=0 the second time, the velocity is -vy-1.
  This means that we want to hit the bottom of the y-target-region with this y-velocity:

  -vy-1 = yMin, so we have vy = -yMin-1

  which means the peak is: -yMin * (-yMin-1) / 2

 -}

instance Solution Day17 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    ((xMin, xMax), (yMin, yMax)) <- consumeOrDie rangeP . head . lines <$> getInputS
    let isInTarget (P (V2 x y)) = xMin <= x && x <= xMax && yMin <= y && y <= yMax
        searchSpace = do
          x <- [1 .. xMax]
          y <- [-2000 .. 2000]
          let trajectory = simulateTill xMax (V2 x y)
          guard $ any isInTarget trajectory
          pure (maximum (fmap (\(P (V2 _ y')) -> y') trajectory))
    print (-yMin * (-yMin-1) `quot` 2)
    print (length searchSpace)

