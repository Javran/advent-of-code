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
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2021.Day11
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
import Text.ParserCombinators.ReadP hiding (count, many)

data Day11 deriving (Generic)

chToInt :: Char -> Int
chToInt ch = ord ch - ord '0'

type Coord = (Int, Int)

type OctoMap = M.Map Coord Int

adjacents :: Coord -> [Coord]
adjacents c@(x, y) = do
  x' <- [x -1 .. x + 1]
  y' <- [y -1 .. y + 1]
  let c' = (x', y')
  guard $ c' /= c
  pure c'

flashingSet :: OctoMap -> S.Set Coord -> S.Set Coord
flashingSet m alreadyFlashing = S.difference (M.keysSet m') alreadyFlashing
  where
    m' = M.filterWithKey (\coord v -> v + (fromMaybe 0 (adjFlashes M.!? coord)) > 9) m
    adjFlashes = M.fromListWith (+) do
      coord <- S.toList alreadyFlashing
      coord' <- adjacents coord
      pure (coord', 1 :: Int)

flashAux :: OctoMap -> S.Set Coord -> OctoMap
flashAux m s =
  if S.null newlyFlashing
    then
      let adjFlashes = M.fromListWith (+) do
            coord <- S.toList s
            coord' <- adjacents coord
            pure (coord', 1 :: Int)
       in M.mapWithKey (\coord v -> v + (fromMaybe 0 (adjFlashes M.!? coord))) m
    else flashAux m (S.union newlyFlashing s)
  where
    newlyFlashing = flashingSet m s

flash :: OctoMap -> (S.Set Coord, OctoMap)
flash m = (M.keysSet $ M.filter (\v -> v > 9) m', M.map (\v -> if v > 9 then 0 else v) m')
  where
    m' = flashAux m S.empty

incrAllBy1 :: OctoMap -> OctoMap
incrAllBy1 = M.map succ

step :: OctoMap -> (Int, OctoMap)
step m = (S.size s', m')
  where
    (s', m') = flash . incrAllBy1 $ m

instance Solution Day11 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (fmap chToInt) . lines <$> getInputS
    let m :: OctoMap
        m = M.fromList do
          (r, row) <- zip [0 ..] xs
          (c, x) <- zip [0 ..] row
          pure ((r, c), x)
    let progression =
          unfoldr
            (\curM ->
               let w@(_, m') = step curM
                in Just (w, m'))
            m
    answerShow $ sum $ fmap fst $ take 100 progression
    let (ans :: Int , _) : _ =
          dropWhile (not . all (== 0) . snd . snd) $ zip [1 ..] progression
    answerShow ans
