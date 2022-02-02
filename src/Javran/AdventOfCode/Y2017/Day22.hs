{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Javran.AdventOfCode.Y2017.Day22
  (
  )
where

import Control.Monad
import Control.Monad.RWS.CPS
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Javran.AdventOfCode.GridSystem.RowThenCol.Uldr
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.TestExtra (consumeExtra)

data Day22 deriving (Generic)

type Nodes = S.Set Coord

type WorldState = (Nodes, (Coord, Dir))

type Sim = RWS () (Sum Int) WorldState

parseFromRaw :: [String] -> Nodes
parseFromRaw xs =
  if odd rows && odd cols
    then S.fromDistinctAscList do
      (r, rs) <- zip [- (halve rows) ..] xs
      (c, '#') <- zip [- (halve cols) ..] rs
      pure (r, c)
    else error "expected rows and cols to both be odd."
  where
    rows = length xs
    cols = length (head xs)

burst :: Sim ()
burst = do
  infected <- gets (\(ns, (cur, _)) -> S.member cur ns)
  modify (second . second $ if infected then turnRight else turnLeft)
  do
    cur <- gets (fst . snd)
    if infected
      then modify (first (S.delete cur))
      else do
        tell 1
        modify (first (S.insert cur))
  modify (second (\(cur, d) -> (applyDir d cur, d)))

data NodeState2 = Weakened | Infected | Flagged deriving (Eq)

type Sim2 = RWS () (Sum Int) WorldState2

type WorldState2 = (M.Map Coord NodeState2, (Coord, Dir))

burst2 :: Sim2 ()
burst2 = do
  curNode <-
    state
      (\(ns, vc@(cur, _)) ->
         let (old, ns') =
               M.alterF
                 (\mOldVal ->
                    ( mOldVal
                    , case mOldVal of
                        Nothing -> Just Weakened
                        Just Weakened -> Just Infected
                        Just Infected -> Just Flagged
                        Just Flagged -> Nothing
                    ))
                 cur
                 ns
          in (old, (ns', vc)))
  modify
    (second . second $ case curNode of
       Nothing -> turnLeft
       Just Weakened -> id
       Just Infected -> turnRight
       Just Flagged -> oppositeDir)
  when (curNode == Just Weakened) do tell 1
  modify (second (\(cur, d) -> (applyDir d cur, d)))

instance Solution Day22 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (ex, rawInput) <- consumeExtra getInputS
    let ns = parseFromRaw . lines $ rawInput
        vc = ((0, 0), U)
    do
      let (_, _, Sum ans) = runRWS (replicateM 10000 burst) () (ns, vc)
      answerShow ans
    do
      let m = M.fromSet (\_ -> Infected) ns
          iters = case ex of
            Nothing -> 10_000_000
            Just _ -> 10000
          (_, _, Sum ans) = runRWS (replicateM iters burst2) () (m, vc)
      answerShow ans
