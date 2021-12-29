{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2018.Day7
  (
  )
where

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Writer.CPS
import Data.Char
import qualified Data.DList as DL
import Data.List
import qualified Data.List.Ordered as LOrd
import qualified Data.Map.Strict as M
import qualified Data.PSQueue as PQ
import qualified Data.Set as S
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day7 deriving (Generic)

type StepDep = (Char, Char)

stepDepP :: ReadP StepDep
stepDepP =
  (,)
    <$> (string "Step " *> nextCharP)
      <*> (string " must be finished before step "
             *> nextCharP <* string " can begin.")

topologicalSort :: Graph -> InDegs -> PQ.PSQ Char Char -> [Char]
topologicalSort graph inDegs q0 = case PQ.minView q0 of
  Nothing -> []
  Just (_ PQ.:-> node, q1) ->
    let (inDegs', enqueues) = decreaseInDegsNextOf graph node inDegs
        q2 = foldr (\n' -> PQ.insert n' n') q1 enqueues
     in node : topologicalSort graph inDegs' q2

type Graph = M.Map Char ([] Char)

type InDegs = M.Map Char Int -- invariant: value always > 0.

decreaseInDegsNextOf :: Graph -> Char -> InDegs -> (InDegs, [] Char)
decreaseInDegsNextOf g n inDegs =
  second DL.toList $
    runWriter
      (foldM
         (\m n' ->
            M.alterF
              (\case
                 Nothing -> pure Nothing
                 Just v ->
                   if v == 1
                     then Nothing <$ tell (DL.singleton n')
                     else pure $ Just (v -1))
              n'
              m)
         inDegs
         nexts)
  where
    nexts = fromMaybe [] (g M.!? n)

decrInDegs :: Applicative f => (Char -> f ()) -> Char -> InDegs -> f InDegs
decrInDegs onDel w =
  M.alterF
    (\case
       Nothing -> pure Nothing
       Just v ->
         if v == 1
           then Nothing <$ onDel w
           else pure $ Just (v -1))
    w

data WorkState = WorkState
  { wsTime :: Int
  , wsProcessing :: [(Int, Char)]
  , wsInDegs :: InDegs
  , wsTodos :: S.Set Char
  }
  deriving (Show)

tick :: Graph -> Int -> (Char -> Int) -> State WorkState Int
tick g capacity requiredTime = do
  curTime <- gets wsTime
  do
    ps <- gets wsProcessing
    let (done, ps') = span ((<= curTime) . fst) ps
    modify \ws -> ws {wsProcessing = ps'}
    modify \ws@WorkState {wsTodos, wsInDegs} ->
      ws
        { wsTodos =
            S.difference wsTodos (S.fromList (fmap snd done))
        , wsInDegs =
            let upd work curInDegs =
                  foldr
                    (M.alter
                       (\case
                          Nothing -> Nothing
                          Just v -> if v == 1 then Nothing else Just (v -1)))
                    curInDegs
                    nexts
                  where
                    nexts = fromMaybe [] (g M.!? work)
             in foldr upd wsInDegs (fmap snd done)
        }
  allDone <- gets \WorkState {wsProcessing, wsTodos} -> null wsProcessing && null wsTodos
  if allDone
    then pure curTime
    else do
      -- see if we can schedule new work.
      fullness <- gets (length . wsProcessing)
      curWork <- gets (fmap snd . wsProcessing)
      let extraAtMost = capacity - fullness
      availableWorks <- gets \WorkState {wsTodos, wsInDegs} ->
        S.toAscList $ S.filter (\w -> (w `M.notMember` wsInDegs) && w `notElem` curWork) wsTodos
      let willSchedule =
            sortBy (comparing fst) $
              fmap (\ch -> (curTime + requiredTime ch, ch)) $ take extraAtMost availableWorks
      modify \ws ->
        ws
          { wsProcessing = LOrd.unionBy (comparing fst) (wsProcessing ws) willSchedule
          }
      newTime <- gets (fst . head . wsProcessing)
      modify \ws -> ws {wsTime = newTime}
      tick g capacity requiredTime

instance Solution Day7 where
  solutionRun _ SolutionContext {getInputS, answerS, answerShow} = do
    (extraOps, rawInput) <- consumeExtraLeadingLines <$> getInputS
    let xs = fmap (consumeOrDie stepDepP) . lines $ rawInput
    let graph = M.fromListWith (<>) do
          (sFrom, sTo) <- xs
          pure (sFrom, [sTo])
        allWorks = S.fromList do
          (sFrom, sTo) <- xs
          [sFrom, sTo]
        inDegs = M.fromListWith (+) do
          (_sFrom, sTo) <- xs
          pure (sTo, 1 :: Int)
        initQ =
          PQ.fromList $
            concatMap
              (\n -> case inDegs M.!? n of
                 Nothing -> [n PQ.:-> n]
                 Just _ -> [])
              $ M.keys graph
    answerS (topologicalSort graph inDegs initQ)
    let (workers, requiredTimeIncr) = case extraOps of
          Nothing -> (5, 60)
          Just ys ->
            let [w, r] = fmap (read @Int) ys
             in (w, r)
        requiredTime ch = ord ch - ord 'A' + 1 + requiredTimeIncr
    answerShow $
      evalState
        (tick graph workers requiredTime)
        WorkState
          { wsTime = 0
          , wsProcessing = []
          , wsInDegs = inDegs
          , wsTodos = allWorks
          }
