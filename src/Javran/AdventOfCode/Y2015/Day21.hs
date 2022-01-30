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
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2015.Day21
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Loops (untilJust)
import Control.Monad.State.Strict
import Data.Char
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Monoid hiding (First, Last)
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Debug.Trace
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day21 deriving (Generic)

-- short for Hit points, damage, and armor
type Hda = (Int, (Int, Int))

hdaP :: ReadP Hda
hdaP = do
  let nl = char '\n'
  h <- strP "Hit Points: " *> decimal1P <* nl
  d <- strP "Damage: " *> decimal1P <* nl
  a <- strP "Armor: " *> decimal1P <* nl
  pure (h, (d, a))

attack :: Hda -> Hda -> Maybe Hda
attack (_, (dmg, _)) (eeHp, eeK@(_, eeArmor)) =
  (eeHp', eeK) <$ guard (eeHp' > 0)
  where
    eeHp' = eeHp - dmgDealt
    dmgDealt = max (dmg - eeArmor) 1

data Side = Player | Boss deriving (Show, Eq)

simulate :: Side -> State (Hda, Hda) (Maybe Side)
simulate attackingSide = do
  let _ker = case attackingSide of
        Player -> _1
        Boss -> _2
      _kee = case attackingSide of
        Player -> _2
        Boss -> _1
  (ker, kee) <- gets (\v -> (v ^. _ker, v ^. _kee))
  case attack ker kee of
    Nothing ->
      -- attackee is dead.
      pure $ Just attackingSide
    Just kee' -> do
      modify (& _kee .~ kee')
      pure Nothing

instance Solution Day21 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    boss' <- consumeOrDie hdaP <$> getInputS
    let player = (8, (5, 5))
        boss = (12, (7, 2))
        r =
          runState
            (do
               let xs = fmap simulate (cycle [Player, Boss])
               fix
                 (\go (y : ys) -> do
                    r <- y
                    case r of
                      Nothing -> go ys
                      Just w -> pure w)
                 xs)
            (player, boss)
    print r
