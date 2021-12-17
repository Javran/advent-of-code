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

module Javran.AdventOfCode.Y2019.Day18
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

data Day18 deriving (Generic)

{-
  Notes:

  Looks like all those maps have some properties that we can take advantage of:

  - there is no 2x2 open cells in the entire map, meaning that there is probably
    only handful of ways (I'll bet 1 or 2 without even looking into details)
    to go from one place to another.

  - if we only look at empty cells,
    there are many tunnels (degree of 2) and dead ends (degree of 1), that
    we can probably get rid of while keeping necessary info of the map intact.

  So instead of performing some search on the input map, we can condense it
  into a graph whose nodes are map cells and edges encode distances between nodes.

  Let's just first do this simplification step and see how many things are left.

 -}

instance Solution Day18 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap id . lines <$> getInputS
    mapM_ print xs
