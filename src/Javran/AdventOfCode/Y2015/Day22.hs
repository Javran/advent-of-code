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

module Javran.AdventOfCode.Y2015.Day22
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Control.Monad.Loops
import Control.Monad.RWS.CPS
import Data.Char
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Monoid hiding (First, Last)
import qualified Data.PSQueue as PQ
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day22 deriving (Generic)

bossInfoP :: ReadP (Int, Int)
bossInfoP = do
  let nl = char '\n'
  h <- strP "Hit Points: " *> decimal1P <* nl
  d <- strP "Damage: " *> decimal1P <* nl
  pure (h, d)

data Effect
  = EShield
  | EPoison
  | ERecharge
  deriving (Eq, Ord, Show)

data Action
  = MagicMissile
  | Drain
  | Shield
  | Poison
  | Recharge
  deriving (Eq, Ord, Show)

manaCost :: Action -> Int
manaCost = \case
  MagicMissile -> 53
  Drain -> 73
  Shield -> 113
  Poison -> 173
  Recharge -> 229

data Side = Player | Boss deriving (Eq, Ord, Show)

data GameState = GameState
  { gsBoss :: Int -- hp
  , gsPlayerHp :: Int
  , gsPlayerMana :: Int
  , gsEffects :: M.Map Effect Int
  , gsSide :: Side
  , gsPremoves :: [Action] -- pre-determined player moves
  }
  deriving (Eq, Ord, Show)

type M =
  RWST
    Int -- boss damage
    (Sum Int) -- mana used
    GameState
    []

oneTurn :: M (Maybe Side)
oneTurn = do
  armor <- do
    effects <- gets gsEffects
    let armor = if M.member EShield effects then 7 else 0
    when (M.member EPoison effects) do
      modify (\gs -> gs {gsBoss = gsBoss gs - 3})
    when (M.member ERecharge effects) do
      modify (\gs -> gs {gsPlayerMana = gsPlayerMana gs + 101})
    pure armor
  bossDead <- gets ((<= 0) . gsBoss)
  modify (\gs -> gs {gsEffects = M.filter (> 0) $ M.map pred $ gsEffects gs})
  if bossDead
    then pure $ Just Player
    else
      gets gsSide >>= \case
        Player -> do
          mana <- gets gsPlayerMana
          allActions <- do
            effects <- gets gsEffects
            pm <- gets gsPremoves
            actions0 <- case pm of
              [] -> pure [MagicMissile, Drain, Shield, Poison, Recharge]
              m : pm' -> do
                modify (\gs -> gs {gsPremoves = pm'})
                pure [m]
            let conflicts =
                  [Shield | M.member EShield effects]
                    <> [Poison | M.member EPoison effects]
                    <> [Recharge | M.member ERecharge effects]
                actions1 = actions0 \\ conflicts
                actions2 = filter ((<= mana) . manaCost) actions1
            pure actions2
          if null allActions
            then pure $ Just Boss
            else do
              action <- lift allActions
              let mCost = manaCost action
              tell (Sum mCost)
              modify (\gs -> gs {gsPlayerMana = mana - mCost})
              case action of
                MagicMissile ->
                  modify (\gs -> gs {gsBoss = gsBoss gs - 4})
                Drain ->
                  modify
                    (\gs ->
                       gs
                         { gsBoss = gsBoss gs - 2
                         , gsPlayerHp = gsPlayerHp gs + 2
                         })
                Shield ->
                  modify (\gs -> gs {gsEffects = M.insert EShield 6 $ gsEffects gs})
                Poison ->
                  modify (\gs -> gs {gsEffects = M.insert EPoison 6 $ gsEffects gs})
                Recharge ->
                  modify (\gs -> gs {gsEffects = M.insert ERecharge 5 $ gsEffects gs})
              modify (\gs -> gs {gsSide = Boss})
              bossDead' <- gets ((<= 0) . gsBoss)
              pure $ if bossDead' then Just Player else Nothing
        Boss -> do
          dmg <- ask
          pHp <- gets gsPlayerHp
          let dmgDealt = max 1 (dmg - armor)
              pHp' = pHp - dmgDealt
          modify (\gs -> gs {gsPlayerHp = pHp', gsSide = Player})
          pure
            if pHp' <= 0
              then Just Boss
              else Nothing

oneTurn2 :: M (Maybe Side)
oneTurn2 =
  gets gsSide >>= \case
    Player -> do
      modify (\gs -> gs {gsPlayerHp = gsPlayerHp gs - 1})
      hp <- gets gsPlayerHp
      if hp <= 0
        then pure $ Just Boss
        else oneTurn
    _ -> oneTurn

type SimState = (GameState, Maybe Side)

{-
  Turns a simulate action into a step function.
 -}
mkStep :: Int -> M (Maybe Side) -> SimState -> [(SimState, Int)]
mkStep bossDmg sim (gs, ms) = case ms of
  Just _ ->
    -- no expansion if the game is already concluded.
    []
  Nothing ->
    let nexts = runRWST sim bossDmg gs
     in fmap (\(a, s, Sum w) -> ((s, a), w)) nexts

prioSearch
  :: (SimState -> [(SimState, Int)])
  -> S.Set SimState
  -> PQ.PSQ (GameState, Maybe Side) Int
  -> [(Side, Int)]
prioSearch step = fix \go discovered q0 -> case PQ.minView q0 of
  Nothing -> []
  Just (ss@(_, ms) PQ.:-> mana, q1) ->
    case ms of
      Just side -> (side, mana) : go discovered q1
      Nothing ->
        let nexts = do
              (next, d) <- step ss
              guard $ S.notMember next discovered
              pure (next, mana + d)
            discovered' = foldr (\(next, _) -> S.insert next) discovered nexts
            q2 = foldr (uncurry PQ.insert) q1 nexts
         in go discovered' q2

{-
  Play a simulation until conclusion.
  After premoves are exhausted, all play decisions are non-deterministic.
 -}
_play :: Int -> GameState -> M (Maybe Side) -> [] (Side, GameState, Sum Int)
_play bossDmg initGs simTurn =
  runRWST
    (untilJust simTurn)
    bossDmg
    initGs

solve :: Int -> GameState -> M (Maybe Side) -> [Int]
solve bossDmg initGs simTurn = do
  (Player, mana) <- prioSearch step (S.singleton initSs) (PQ.singleton initSs 0)
  pure mana
  where
    step = mkStep bossDmg simTurn
    initSs = (initGs, Nothing)

instance Solution Day22 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (bossHp, bossDmg) <- consumeOrDie bossInfoP <$> getInputS
    let initGs =
          GameState
            { gsBoss = bossHp
            , gsPlayerHp = 50
            , gsPlayerMana = 500
            , gsEffects = M.empty
            , gsSide = Player
            , gsPremoves = []
            }

    answerShow $ head $ solve bossDmg initGs oneTurn
    answerShow $ head $ solve bossDmg initGs oneTurn2
