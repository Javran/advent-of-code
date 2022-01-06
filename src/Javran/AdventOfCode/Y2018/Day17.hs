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

module Javran.AdventOfCode.Y2018.Day17
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
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

data Day17 deriving (Generic)

type Coord = (Int, Int)

inputLineP :: ReadP [(Int, Int)]
inputLineP = do
  (cNext, mk) <-
    (('y', \x ys -> [(x, y) | y <- ys]) <$ char 'x')
      <++ (('x', (\y xs -> [(x, y) | x <- xs])) <$ char 'y')
  _ <- char '='
  u <- decimal1P
  _ <- string ", "
  _ <- char cNext
  _ <- char '='
  vFrom <- decimal1P
  _ <- string ".."
  vTo <- decimal1P
  pure (mk u [vFrom .. vTo])

instance Solution Day17 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (consumeOrDie inputLineP) . lines <$> getInputS
    let coords = S.fromList xs
    mapM_ print xs
