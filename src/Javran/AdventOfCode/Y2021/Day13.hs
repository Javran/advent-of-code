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

module Javran.AdventOfCode.Y2021.Day13
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

data Day13 deriving (Generic)

type Dot = (Int, Int)

data FoldAlong = AlongX Int | AlongY Int deriving (Show)

foldInstrP :: ReadP FoldAlong
foldInstrP = do
  _ <- string "fold along "
  (string "y=" *> (AlongY <$> decimal1P))
    <++ (string "x=" *> (AlongX <$> decimal1P))


foldAlongX :: Int -> S.Set Dot -> S.Set Dot
foldAlongX hw xs = S.fromList do
  (x,y) <- S.toList xs
  if
    | x < hw -> pure (x,y)
    | x == hw -> errInvalid
    | x > hw -> pure (hw*2-x, y)

foldAlongY :: Int -> S.Set Dot -> S.Set Dot
foldAlongY hh xs = S.fromList do
  (x,y) <- S.toList xs
  if
    | y < hh -> pure (x,y)
    | y == hh -> errInvalid
    | y > hh -> pure (x, hh*2-y)

appFoldInstr :: FoldAlong -> S.Set Dot -> S.Set Dot
appFoldInstr = \case
  AlongX x -> foldAlongX x
  AlongY y -> foldAlongY y

instance Solution Day13 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    [dotsRaw, foldInstrsRaw] <- splitOn [""] . lines <$> getInputS
    let dots = fmap ((\[x, y] -> (x, y)) . fmap (read @Int) . splitOn ",") dotsRaw
        foldInstrs = fmap (consumeOrDie foldInstrP) foldInstrsRaw
    answerShow $ S.size $  (appFoldInstr (head foldInstrs) (S.fromList dots))
    let result = foldl (\acc i -> appFoldInstr i acc) (S.fromList dots) foldInstrs
        Just ((Min minX, Max maxX), (Min minY, Max maxY)) =
          foldMap (\(x, y) -> Just ((Min x, Max x), (Min y, Max y))) $ S.toList result
    forM_ [minY..maxY] $ \y -> do
      let row = fmap (\x -> let c = (x,y) in if S.member c result then '█' else ' ') [minX .. maxX]
      putStrLn row
