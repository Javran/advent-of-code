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
import Linear.Affine
import Linear.V2
import Text.ParserCombinators.ReadP hiding (count, many)

data Day10 deriving (Generic)

type Coord = Point V2 Int

instance Solution Day10 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    raws <- lines <$> getInputS
    let asteroids :: [Coord]
        asteroids = do
          (y, rs) <- zip [0 ..] raws
          (x, '#') <- zip [0 ..] rs
          pure $ P $ V2 x y
        stations :: [(Point V2 Int, M.Map (V2 Int) [Coord])]
        stations = do
          (c@(P (V2 x0 y0)), others) <- pick asteroids
          pure
            ( c
            , M.fromListWith (<>) do
                cur@(P (V2 x1 y1)) <- others
                let (dx, dy) = (x1 - x0, y1 - y0)
                let slope@(sx, sy) =
                      if dy == 0
                        then (1, 0)
                        else
                          let r = dx % dy
                           in (numerator r, denominator r)
                    sign = signum $ - sx * dx - sy * dy
                pure (V2 (sy * sign) (sx * sign), [cur])
            )
        bestStation@((P (V2 bestX bestY)), bestM) = maximumBy (comparing (M.size . snd)) stations
    answerShow $ M.size bestM
    let toPos v = if v >= pi / 2 then v else v + pi * 2
        stationDist (P (V2 x y)) = abs (x - bestX) + abs (y - bestY)
        ordered =
          fmap snd $
            (fmap . second) (sortOn stationDist) $
              sortOn
                ((\(V2 x y) -> toPos $ atan2 @Double (fromIntegral x) (fromIntegral y)) . fst)
                $ M.toList bestM
        elimOrders =
          concat $
            unfoldr
              (\cur -> do
                 _ : _ <- pure cur
                 let (hdsPre, tlsPre) = unzip $ fmap (splitAt 1) cur
                     tls = filter (not . null) tlsPre
                 Just (concat hdsPre, tls))
              ordered
    let (P (V2 x y)) =
          if length elimOrders < 200
            then last $ elimOrders
            else elimOrders !! (200 - 1)
    answerShow (x * 100 + y)
