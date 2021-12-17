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
    take 3000 $
      takeWhile (\(P (V2 x _), _) -> x <= xMax) $
        iterate step (0, initVel)

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
    print (maximum searchSpace)
    print (length searchSpace)
