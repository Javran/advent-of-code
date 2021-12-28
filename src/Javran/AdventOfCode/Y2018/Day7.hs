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

topologicalSort graph inDegrees q0 = case PQ.minView q0 of
  Nothing -> []
  Just (_ PQ.:-> (inDeg, node), q1) ->
    if inDeg > 0
      then []
      else
        let nextNodes = do
              Just ns <- pure (graph M.!? node)
              S.toList ns
            inDegrees' = foldr upd inDegrees nextNodes
              where
                upd node' =
                  M.alter
                    (\case
                       Nothing -> Nothing
                       Just v -> if v == 1 then Nothing else Just (v -1))
                    node'
            q2 = foldr upd q1 nextNodes
              where
                upd node' = case inDegrees' M.!? node' of
                  Nothing -> PQ.insert node' (0, node')
                  Just _ -> id
         in node : topologicalSort graph inDegrees' q2

instance Solution Day7 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerS} = do
    xs <- fmap (consumeOrDie stepDepP) . lines <$> getInputS
    let graph = M.fromListWith (<>) do
          (sFrom, sTo) <- xs
          pure (sFrom, S.singleton sTo)
        inDegrees = M.fromListWith (+) do
          (sFrom, sTo) <- xs
          pure (sTo, 1 :: Int)
        initNodes =
          filter
            (\n -> case inDegrees M.!? n of
               Nothing -> True
               Just _ -> False)
            $ M.keys graph
    mapM_ print (M.toList graph)
    answerS (topologicalSort graph inDegrees $ PQ.fromList $ fmap (\n -> n PQ.:-> (0, n)) initNodes)
