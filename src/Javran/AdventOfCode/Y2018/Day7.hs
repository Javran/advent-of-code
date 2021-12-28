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

module Javran.AdventOfCode.Y2018.Day7
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Control.Monad.Writer.CPS
import Data.Char
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Monoid
import qualified Data.PSQueue as PQ
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
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
    let nextNodes = do
          Just ns <- pure (graph M.!? node)
          ns
        (inDegs', enqueues) = runWriter (foldM upd inDegs nextNodes)
          where
            upd m node' =
              M.alterF
                (\case
                   Nothing -> pure Nothing
                   Just v ->
                     if v == 1
                       then Nothing <$ tell (Endo $ PQ.insert node' node')
                       else pure $ Just (v -1))
                node'
                m
        q2 = appEndo enqueues q1
     in node : topologicalSort graph inDegs' q2

type Graph = M.Map Char ([] Char)

type InDegs = M.Map Char Int -- invariant: value always > 0.

instance Solution Day7 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerS} = do
    xs <- fmap (consumeOrDie stepDepP) . lines <$> getInputS
    let graph = M.fromListWith (<>) do
          (sFrom, sTo) <- xs
          pure (sFrom, [sTo])
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
