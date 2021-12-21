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

module Javran.AdventOfCode.Y2021.Day21
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
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
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day21 deriving (Generic)

playerP :: ReadP (Int, Int)
playerP =
  (,) <$> (string "Player " *> decimal1P)
    <*> (string " starting position: " *> decimal1P)

data GameState = GameState
  { gsDie :: [(Int, Int)]
  , gsPlayer :: IM.IntMap (Int, Sum Int)
  }

stepPlayer :: Int -> State GameState (Maybe Int)
stepPlayer who = do
  pt <- diceRoll
  advance (sum (fmap snd pt))
  score <- gets (snd . (IM.! who) . gsPlayer)
  pure do
    guard $ score >= 1000
    pure who
  where
    diceRoll =
      state
        (\gs@GameState {gsDie} ->
           let (ls, die') = splitAt 3 gsDie
            in (ls, gs {gsDie = die'}))
    advance pt = do
      let upd (pos, score) = (pos', score <> Sum pos')
            where
              pos' = let x = (pos + pt) `rem` 10 in if x == 0 then 10 else x
      modify (\gs@GameState {gsPlayer} -> gs {gsPlayer = IM.adjust upd who gsPlayer})

stepTillWon (p : ps) = do
  mWinner <- stepPlayer p
  case mWinner of
    Just w -> pure w
    Nothing -> stepTillWon ps

type GameState2 = (Bool {-p1's turn if false-}, ((Int {- pos -}, Int {- score -}), (Int, Int)))

allSteps :: IM.IntMap Int
allSteps = IM.fromListWith (+) do
  x <- sum <$> replicateM 3 [1, 2, 3]
  pure (x, 1)

step2 :: GameState2 -> M.Map GameState2 Int
step2 (False, ((p1Loc, p1Score), p2)) = M.fromListWith (+) do
  (step, univCount) <- IM.toList allSteps
  let p1Loc' = let x = (p1Loc + step) `rem` 10 in if x == 0 then 10 else x
      p1Score' = p1Score + p1Loc'
  pure ((True, ((p1Loc', p1Score'), p2)), univCount)
step2 (True, (p1, (p2Loc, p2Score))) = M.fromListWith (+) do
  (step, univCount) <- IM.toList allSteps
  let p2Loc' = let x = (p2Loc + step) `rem` 10 in if x == 0 then 10 else x
      p2Score' = p2Score + p2Loc'
  pure ((False, (p1, (p2Loc', p2Score'))), univCount)

stepUniv :: M.Map GameState2 Int -> ((Sum Int, Sum Int), M.Map GameState2 Int)
stepUniv m = (w, inconclusives)
  where
    w =
      mconcat $
        mapMaybe
          (\(univ, count) -> do
             p2Won <- isConclusive univ
             pure $ if p2Won then (0, Sum count) else (Sum count, 0))
          (M.toList conclusives)
    conclusives, inconclusives :: M.Map GameState2 Int
    (conclusives, inconclusives) = M.partitionWithKey (\k _v -> isJust (isConclusive k)) stepped

    isConclusive :: GameState2 -> Maybe Bool
    isConclusive (_, ((_, p1Score), (_, p2Score)))
      | p1Score >= 21 = Just False
      | p2Score >= 21 = Just True
      | otherwise = Nothing
    stepped :: M.Map GameState2 Int
    stepped = M.unionsWith (+) do
      (univ, count) <- M.toList m
      let univ' = M.map (* count) $ step2 univ
      pure univ'

instance Solution Day21 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    [(1, p1), (2, p2)] <- fmap (consumeOrDie playerP) . lines <$> getInputS
    let initSt = GameState {gsDie = zip [0 ..] (cycle [1 .. 100]), gsPlayer = IM.fromList [(1, (p1, 0)), (2, (p2, 0))]}
        (winner, GameState {gsDie = (dieCount, _) : _, gsPlayer = pFin}) = runState (stepTillWon (cycle [1, 2])) initSt
        loser = 3 - winner
        initAllUniv = (M.singleton (False, ((p1, 0), (p2, 0))) 1)
        univs =
          unfoldr
            (\univ -> do
               guard $ not $ null univ
               let (w, univ') = stepUniv univ
               pure (w, univ'))
            initAllUniv
        ans2 = max l r
          where
            (Sum l, Sum r) = mconcat univs
    answerShow (dieCount * (getSum $ snd $ pFin IM.! loser))
    answerShow ans2
