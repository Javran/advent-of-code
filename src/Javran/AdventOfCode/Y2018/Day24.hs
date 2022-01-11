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
import Data.Coerce
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
import Text.Printf

data Day24 deriving (Generic)

newtype DamageType = DamageType Int deriving (Show)

newtype ImmuneWeak = ImmuneWeak (IS.IntSet, IS.IntSet) deriving (Show)

data ArmyGroup = ArmyGroup
  { agCount :: Int
  , agHp :: Int
  , agImmuneWeak :: ImmuneWeak
  , agAttack :: (Int, DamageType)
  , agInit :: Int
  }
  deriving (Show)

armyGroupP :: ReadP ((String -> DamageType) -> ArmyGroup, S.Set String)
armyGroupP = do
  agCount <- decimal1P <* string " units each with "
  agHp <- decimal1P <* string " hit points "
  let dmgTyP = munch1 isAlpha
      dmgTysP = dmgTyP `sepBy1` string ", "
      immuneWeakP =
        mconcat
          <$> ((immuneToP <++ weakToP) `sepBy1` string "; ")
      immuneToP =
        ((,mempty) . S.fromList) <$> (string "immune to " *> dmgTysP)
      weakToP =
        ((mempty,) . S.fromList) <$> (string "weak to " *> dmgTysP)
  immuneWeak@(immunes, weaks) <-
    between (char '(') (string ") ") immuneWeakP
      <++ pure mempty
  _ <- string "with an attack that does "
  dmg <- decimal1P <* char ' '
  dmgTy <- dmgTyP <* string " damage at initiative "
  agInit <- decimal1P
  let mkArmy sToI =
        ArmyGroup
          { agCount
          , agHp
          , agImmuneWeak =
              let f :: S.Set String -> IS.IntSet
                  f = IS.fromList . coerce . fmap sToI . S.toList
               in ImmuneWeak $ bimap f f immuneWeak
          , agAttack = (dmg, sToI dmgTy)
          , agInit
          }
  pure (mkArmy, S.unions [immunes, weaks, S.singleton dmgTy])

inputP :: ReadP (([ArmyGroup], [ArmyGroup]), DamageType -> String)
inputP = do
  let nl = void (char '\n')
  _ <- string "Immune System:\n"
  (immuneAgs, acc0) <- unzip <$> many1 (armyGroupP <* nl)
  nl
  _ <- string "Infection:\n"
  (infectionAgs, acc1) <- unzip <$> many1 (armyGroupP <* nl)
  let allDmgTypes = S.toList $ S.unions $ acc0 <> acc1
      dmgTypes = V.fromList allDmgTypes
      revDmgTypes = M.fromList (zip allDmgTypes [0 ..])
      sToI = DamageType . (revDmgTypes M.!)
      iToS (DamageType i) = dmgTypes V.! i
  pure ((fmap ($ sToI) immuneAgs, fmap ($ sToI) infectionAgs), iToS)

pprParsed ((xs, ys), iToS) = do
  let pprGroup
        ArmyGroup
          { agCount
          , agHp
          , agImmuneWeak =
            ImmuneWeak (im, wk)
          , agAttack = (atkVal, atkTy)
          , agInit
          } = do
          printf
            "  Units: %d, Hps: %d, Attack: %d %s, Init: %d\n"
            agCount
            agHp
            atkVal
            (iToS atkTy)
            agInit
          unless (IS.null im) do
            putStrLn $
              "    Immune to: "
                <> intercalate ", " (fmap (iToS . DamageType) $ IS.toList im)
          unless (IS.null wk) do
            putStrLn $
              "    Weak to: "
                <> intercalate ", " (fmap (iToS . DamageType) $ IS.toList wk)
  putStrLn "Immune system:"
  mapM_ pprGroup xs
  putStrLn "Infection:"
  mapM_ pprGroup ys

instance Solution Day24 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    inp <- consumeOrDie inputP <$> getInputS
    pprParsed inp
