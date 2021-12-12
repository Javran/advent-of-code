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

module Javran.AdventOfCode.Y2019.Day10
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
import Data.Ratio
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, many)

data Day10 deriving (Generic)

type Coord = (Int, Int)

instance Solution Day10 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- lines <$> getInputS
    let asteroids :: [Coord]
        asteroids = do
          (r, rs) <- zip [0 ..] xs
          (c, '#') <- zip [0 ..] rs
          pure (r, c)
        stations = do
          (c@(y0, x0), others) <- pick asteroids
          let vs = fmap (\(y1, x1) -> (y1 - y0, x1 - x0)) others
          pure
            ( c
            , M.fromListWith (<>) do
                cur@(y1, x1) <- others
                let (dy, dx) = (y1 - y0, x1 - x0)
                let slope@(sy, sx) =
                      if dx == 0
                        then (1, 0)
                        else
                          let r = dy % dx
                           in (numerator r, denominator r)
                    sign = signum $ - sx * dx - sy * dy
                pure ((sx * sign, sy * sign), [cur])
            )
        bestStation@((bestY, bestX), bestM) = maximumBy (comparing (M.size . snd)) stations
    -- print (bestCoord, M.size bestM)
    answerShow $ M.size bestM
    let toPos v = if v >= pi / 2 then v else v + pi * 2
        stationDist (y, x) = abs (y - bestY) + abs (x - bestX)
        ordered =
          fmap snd $
            (fmap . second) (sortOn stationDist) $
              sortOn
                ((\(y, x) -> toPos $ atan2 @Double (fromIntegral x) (fromIntegral y)) . fst)
                $ M.toList bestM
        elimOrders =
          unfoldr
            (\cur -> do
               guard $ not $ null cur
               let (hdsPre, tlsPre) = unzip $ fmap (splitAt 1) cur
                   tls = filter (not . null) tlsPre
               Just (concat hdsPre, tls))
            ordered
    let (y, x) =
          if length (concat elimOrders) < 200
            then last $ concat elimOrders
            else concat elimOrders !! (200 - 1)
    answerShow (x * 100 + y)
