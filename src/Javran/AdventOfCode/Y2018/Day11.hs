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

module Javran.AdventOfCode.Y2018.Day11
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import qualified Data.Array.ST as Arr
import qualified Data.Array.Unboxed as UArr
import Data.Char
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day11 deriving (Generic)

type Coord = (Int, Int) -- X and Y

powerLevel :: Int -> Coord -> Int
powerLevel serial (x, y) = p3 - 5
  where
    rackId = x + 10
    p0 = rackId * y
    p1 = p0 + serial
    p2 = p1 * rackId
    p3 = (p2 `quot` 100) `rem` 10

{-
  There's a trick that allows us to do some preprocessing
  and then get sum of consecutive regions for a 1D array:

  let s[0] = 0, s[i] = sum of a[1], a[2], .. a[i] (a is 1-based array).

  then sum of a[m], a[m+1] .. a[n] is just s[n] - s[m-1].

  I guess for doing part 2 in an efficient manner, we can try to
  generalize this idea to 2D.
 -}

computeAccumulated :: (Coord -> Int) -> UArr.UArray (Int, Int) Int
computeAccumulated getVal = Arr.runSTUArray do
  let sz = 300
  arr <- Arr.newArray ((0, 0), (sz, sz)) (0 :: Int)
  forM_ [1 .. sz] \x ->
    forM_ [1 .. sz] \y -> do
      let v = getVal (x, y)
      ~[v0, v1, v2] <- mapM (Arr.readArray arr) [(x -1, y), (x, y -1), (x -1, y -1)]
      Arr.writeArray arr (x, y) $! v + v0 + v1 - v2
  pure arr

instance Solution Day11 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    n <- read @Int <$> getInputS
    let accd = computeAccumulated (powerLevel n)
        querySum (x0, y0) (x1, y1) =
          accd UArr.! (x1, y1)
            - accd UArr.! (x0 -1, y1)
            - accd UArr.! (x1, y0 -1)
            + accd UArr.! (x0 -1, y0 -1)
        ans = maximumBy (comparing snd) do
          x <- [1 .. 300 - 3 + 1]
          y <- [1 .. 300 -3 + 1]
          let topLeft = (x, y)
              totalPower = querySum (x,y) (x+2, y+2)
          pure (topLeft, totalPower)
    print ans
