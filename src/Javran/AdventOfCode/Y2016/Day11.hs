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

module Javran.AdventOfCode.Y2016.Day11
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
import Text.ParserCombinators.ReadP hiding (count, many, get)

data Day11 deriving (Generic)

{-
  I'm not impressed by this poorly written problem description,
  it's almost like it is intentionally so to cause confusion.

  So to summarize and make things a bit more clear, here are my notes:

  - There are microchips (M) and generators (G), existing in pairs,
    in one-to-one correspondence (denoted as G-M pair).

  - When a G-M pair is on the same floor, they together
    forms a shield that protects the microchip.
    (whether either is inside of the elevator is irrelevant).

  - Generator G does damage to microchips M'
    when all of the following is true:

    - G is not paired with M'
    + M' is on the same floor
    + M' is not paired with it's generator G',
      as G'-M' sheilds M' from the damage.

  - Microchips does not interact with each other.

  - There's one elevator:

    + starting from floor 1 and move between 1-4.
    + only capable of moving up or down by 1 at a time.
    + must carry G or M for it to function
    + carries at most 2 of any combination of G or M.

  - The goal is to move everything to 4th floor without damaging
    to any microchips.

 -}

instance Solution Day11 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap id . lines <$> getInputS
    mapM_ print xs