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

module Javran.AdventOfCode.Y2021.Day9
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
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day9 deriving (Generic)

chToInt :: Char -> Int
chToInt ch = ord ch - ord '0'

type Coord = (Int, Int)

neighbors :: Coord -> [] Coord
neighbors c =
  fmap
    ($ c)
    [ first succ
    , first pred
    , second succ
    , second pred
    ]

findBasin :: M.Map Coord Int -> Coord -> State (S.Set Coord) ()
findBasin heights coord = do
  let h :: Int
      h = heights M.! coord
  visited <- get
  if S.member coord visited || h == 9
    then pure ()
    else do
      modify (S.insert coord)
      let coords :: [Coord]
          coords = filter (\c' -> M.member c' heights) (neighbors coord)
      forM_ coords $ \coord' -> do
        let h' = heights M.! coord'
        when (S.notMember coord' visited && h' > h) $
          findBasin heights coord'

instance Solution Day9 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (fmap chToInt) . lines <$> getInputS
    let heights = M.fromList do
          (r, row) <- zip [0 ..] xs
          (c, x) <- zip [0 ..] row
          pure ((r, c), x)
        lowPoints = do
          (coord, h) <- M.toList heights
          let surroundingHs = mapMaybe (\c' -> heights M.!? c') (neighbors coord)
          guard $ all (> h) surroundingHs
          pure (coord, h)
    answerShow $ sum $ fmap (succ . snd) lowPoints
    answerShow $
      product $
        take 3 $
          reverse $ sort do
            (c, _) <- lowPoints
            pure $ S.size $ execState (findBasin heights c) S.empty
