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

module Javran.AdventOfCode.Y2016.Day22
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
import Javran.AdventOfCode.GridSystem.RowThenCol.Uldr
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day22 deriving (Generic)

data Node = Node
  { nSize :: Int
  , nUsed :: Int
  , nAvail :: Int
  , nUsePercent :: Int
  }
  deriving (Show)

nodeP :: ReadP (Coord, Node)
nodeP = do
  strP "/dev/grid/node-x"
  x <- decimal1P
  strP "-y"
  y <- decimal1P <* skipSpaces
  nSize <- decimal1P <* charP 'T' <* skipSpaces
  nUsed <- decimal1P <* charP 'T' <* skipSpaces
  nAvail <- decimal1P <* charP 'T' <* skipSpaces
  nUsePercent <- decimal1P <* charP '%'
  pure ((y, x), Node {nSize, nUsed, nAvail, nUsePercent})

instance Solution Day22 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    nodes <- fmap (consumeOrDie nodeP) . drop 2 . lines <$> getInputS
    let viablePairs = do
          ((_,a), xs0) <- pick nodes
          (_,b) <- xs0
          guard $ nUsed a /= 0 && nUsed a <= nAvail b
          pure (a,b)
    answerShow $ length viablePairs
