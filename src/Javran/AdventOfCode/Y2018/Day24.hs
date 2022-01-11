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

module Javran.AdventOfCode.Y2018.Day24
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

data Day24 deriving (Generic)

newtype DamageType = DamageType Int

newtype ImmuneWeak = ImmuneWeak (IS.IntSet, IS.IntSet)

data ArmyGroup = ArmyGroup
  { agCount :: Int
  , agHp :: Int
  , agImmuneWeak :: ImmuneWeak
  , agAttack :: (Int, DamageType)
  , agInit :: Int
  }

armyGroupP = do
  agCount <- decimal1P <* string " units each with "
  agHp <- decimal1P <* string " hit points "
  let dmgTyP = munch1 isAlpha
      immuneWeakP = (immuneToP <++ weakToP) `sepBy1` string "; "
      immuneToP = Left <$> (string "immune to " *> (dmgTyP `sepBy1` string ", "))
      weakToP = Right <$> (string "weak to " *> (dmgTyP `sepBy1` string ", "))
  tmp <- between (char '(') (string ") ") (Just <$> immuneWeakP) <++ pure Nothing
  _ <- string "with an attack that does "
  dmg <- decimal1P <* char ' '
  dmgTy <- dmgTyP <* string " damage at initiative "
  agInit <- decimal1P
  pure (agCount, agHp, tmp, dmg, dmgTy, agInit)

inputP = do
  let nl = char '\n'
  string "Immune System:\n"
  xs <- many1 (armyGroupP <* nl)
  nl
  string "Infection:\n"
  ys <- many1 (armyGroupP <* nl)
  pure (xs, ys)

instance Solution Day24 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (xs, ys) <- consumeOrDie inputP <$> getInputS
    putStrLn "Immune System:"
    mapM_ print xs
    putStrLn "Infection:"
    mapM_ print ys
