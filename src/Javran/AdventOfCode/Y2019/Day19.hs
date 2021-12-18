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

module Javran.AdventOfCode.Y2019.Day19
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
import Data.Ratio
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2019.IntCode
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day19 deriving (Generic)

type Coord = (Int, Int)

probes :: [Int] -> [Coord] -> IO [(Coord, Int)]
probes code = mapM \c@(x, y) -> do
  (_, [r]) <- liftIO $ runProgram code [x, y]
  pure (c, r)

{-
  TODO:

  - given a y, we want to estimate where is max x that is in the beam.
    this info can be obtained by deducing from 50x50 area in part 1.

  - once (max x, y) is found, probe the size of the square that it can fit.

  - find a y that overshots

  - perform binary search on y

 -}
instance Solution Day19 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    code <- parseCodeOrDie <$> getInputS
    let sz = 50
        area = [(x, y) | y <- [0 .. sz -1], x <- [0 .. sz -1]]
    results <- probes code area
    answerShow $ countLength (\(_, r) -> r == 1) results

    {-
      Assumptions that are probably safe:

     - the beam is bounded by two straight lines,
       some rounding error makes it edge jagged.

     - y = a * x / b, since the beam seems to start from (0,0).

     -}

    do
      let ysWithMaxX = IM.fromListWith (<>) do
            ((x, y), 1) <- results
            guard $ y >= 10 -- to reduce low-denominator noise
            pure (y, Just (Max x))
          Just (MinMax (rLo, rHi)) = mconcat do
            (y, Just (Max x)) <- IM.toList ysWithMaxX
            let p = ((/) @Double `on` fromIntegral)
            pure (Just $ minMax (p x y))
      -- note: lower bound seems to be more reliably inside.

      let binarySearchForMaxX y xL xR = do
            let x = (xL + xR) `quot` 2
            [(_, v)] <- probes code [(x, y)]
            case v of
              1 -> if xL /= x then binarySearchForMaxX y x xR else pure xL
              0 -> if xR /= x then binarySearchForMaxX y xL x else pure xL
              _ -> unreachable

          yLo = 500
          yHi = 5000
          binarySearch2 l r = do
            let y = (l + r) `quot` 2
                xLo = floor (rLo * fromIntegral y)
                xHi = xLo * 2 -- just need some value that is definitely outside.
            xMax <- binarySearchForMaxX y xLo xHi
            [(_, 1), (_, v2)] <- probes code [(xMax, y), (xMax -99, y + 99)]
            case v2 of
              0 -> do
                -- TODO: only trust y. xMax could be out of sync.
                if l /= y
                  then binarySearch2 y r
                  else do
                    xm <- binarySearchForMaxX r xLo xHi
                    pure (xm, r)
              1 -> do
                if r /= y
                  then binarySearch2 l y
                  else do
                    xm <- binarySearchForMaxX r xLo xHi
                    pure (xm, r)
              _ -> unreachable
      (xAns, yAns) <- binarySearch2 yLo yHi
      answerShow ((xAns - 99) * 10000 + yAns)
