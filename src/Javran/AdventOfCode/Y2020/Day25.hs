{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
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

module Javran.AdventOfCode.Y2020.Day25
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Bool
import Data.Char
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Mod
import Data.Monoid
import Data.Proxy
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Natural
import GHC.TypeNats (natVal)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, many)

data Day25

type ModP = Mod 20201227

subject :: ModP
subject = 7

rootMod :: Natural
rootMod = natVal subject

-- https://en.wikipedia.org/wiki/Baby-step_giant-step#The_algorithm
instance Solution Day25 where
  solutionIndex _ = (2020, 25)
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    cPk : dPk : _ <- fmap (fromIntegral @_ @ModP . read @Int) . lines <$> getInputS
    let m :: Int
        m = ceiling (sqrt (fromIntegral rootMod :: Double))
        babyStep :: HM.HashMap Natural Int
        babyStep = HM.fromList do
          j <- [0 .. m -1]
          pure (unMod (subject ^% j), j)
        aPowNegM = subject ^% (- m)
        giantStep gamma i = do
          guard $ i < m
          case HM.lookup (unMod gamma) babyStep of
            Nothing -> giantStep (gamma * aPowNegM) (i + 1)
            Just j -> Just (i * m + j)

    let Just cSk = giantStep cPk 0
    print $ unMod $ dPk ^% cSk
