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

module Javran.AdventOfCode.Y2018.Day18
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Data.Char
import qualified Data.DList as DL
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

data Day18 deriving (Generic)

type Coord = (Int, Int) -- row, col

type CoordSet = S.Set Coord

type World = (CoordSet, CoordSet) -- trees, lumberyard

parseFromRaw :: [String] -> ((Int, Int), World)
parseFromRaw xs = ((rows, cols), bimap convert convert (ts, ls))
  where
    convert = S.fromList . DL.toList
    rows = length xs
    cols = length (head xs)
    (ts, ls) = mconcat do
      (r, row) <- zip [0 ..] xs
      (c, ch) <- zip [0 ..] row
      pure case ch of
        '|' -> (DL.singleton (r, c), mempty)
        '#' -> (mempty, DL.singleton (r, c))
        _ -> mempty

adjacents :: Coord -> [Coord]
adjacents coord@(r, c) = do
  r' <- [r -1 .. r + 1]
  c' <- [c -1 .. c + 1]
  let coord' = (r', c')
  coord' <$ guard (coord' /= coord)

pprWorld :: (Int, Int) -> World -> IO ()
pprWorld (rows, cols) (ts, ls) =
  forM_ [0 .. rows -1] \r -> do
    let render c
          | S.member coord ts = '|'
          | S.member coord ls = '#'
          | otherwise = '.'
          where
            coord = (r, c)
    putStrLn (fmap render [0 .. cols -1])

stepWorld :: (Int, Int) -> World -> World
stepWorld (rows, cols) (ts, ls) = (convert ts', convert ls')
  where
    convert = S.fromList . DL.toList
    (ts', ls') = foldMap stepCoord coords
    stepCoord coord
      | not isTree && not isYard =
        if treeCnt >= 3 then (DL.singleton coord, mempty) else mempty
      | isTree =
        if yardCnt >= 3 then (mempty, DL.singleton coord) else (DL.singleton coord, mempty)
      | isYard =
        if treeCnt >= 1 && yardCnt >= 1 then (mempty, DL.singleton coord) else mempty
      | otherwise = unreachable
      where
        isTree = S.member coord ts
        isYard = S.member coord ls
        treeCnt = fromMaybe 0 (treeContrib M.!? coord)
        yardCnt = fromMaybe 0 (yardContrib M.!? coord)

    coords = S.unions [M.keysSet treeContrib, M.keysSet yardContrib, ts, ls]
    treeContrib = M.fromListWith (+) do
      coord <- S.toList ts
      coord'@(r', c') <- adjacents coord
      guard $ 0 <= r' && r' < rows && 0 <= c' && c' < cols
      pure (coord', 1 :: Int)

    yardContrib = M.fromListWith (+) do
      coord <- S.toList ls
      coord'@(r', c') <- adjacents coord
      guard $ 0 <= r' && r' < rows && 0 <= c' && c' < cols
      pure (coord', 1 :: Int)

findFix seen ~((j, x) : xs) = case seen M.!? x of
  Nothing -> findFix (M.insert x j seen) xs
  Just i -> (i, j)

instance Solution Day18 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap id . lines <$> getInputS
    let (dims, w) = parseFromRaw xs
        progression = iterate (stepWorld dims) w
    do
      let (ts, ls) = progression !! 10
      pprWorld dims (ts, ls)
      answerShow (S.size ts * S.size ls)
    do
      let (i, j) = findFix M.empty (zip [0 ..] progression)
          period = j - i
          n = 1000000000
          i' = i + (n - i) `rem` period
          (ts, ls) = progression !! i'
      answerShow (S.size ts * S.size ls)
