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

module Javran.AdventOfCode.Y2019.Day14
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Control.Monad.Writer.CPS
import Data.Bifunctor
import Data.Bool
import Data.Char
import Data.Either
import Data.Foldable
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.Graph as G
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
import Text.ParserCombinators.ReadP hiding (count, many)

data Day14 deriving (Generic)

type Chem = String

type InputChems = M.Map Chem Int

type Reaction = (InputChems, (Chem, Int))

reactionP :: ReadP Reaction
reactionP = do
  xs <- amountChemP `sepBy1` string ", "
  _ <- string " => "
  y <- amountChemP
  pure (M.fromList xs, y)
  where
    amountChemP = (flip (,)) <$> decimal1P <*> (char ' ' *> munch1 isAlpha)

type ReactionTable = M.Map Chem (InputChems, Int)

{-
  To eliminate a chemical from a demand list is to replace it
  by the reaction producing it, multipled by a factor
  that produces just sufficient amount (it might result in some extras)
 -}
elim :: ReactionTable -> Chem -> M.Map Chem Int -> M.Map Chem Int
elim tbl rmTarget m = case m M.!? rmTarget of
  Just needCnt ->
    let (lhs, rhsCnt) = tbl M.! rmTarget
        factor = ceiling @Double @Int $ ((/) `on` fromIntegral) needCnt rhsCnt
     in M.unionWith (+) (M.delete rmTarget m) (M.map (* factor) lhs)
  Nothing -> m

topologicalSort :: forall f a. (Foldable f, Ord a) => M.Map a (f a) -> [a]
topologicalSort g = fmap ((\(_, k, _) -> k) . nodeFromVertex) sorted
  where
    sorted = G.topSort graph
    (graph, nodeFromVertex, _vertexFromKey) = G.graphFromEdges do
      (k, vs) <- M.toList g
      pure ((), k, toList vs)

solveFuel :: M.Map Chem (InputChems, Int) -> [Chem] -> Int -> Int
solveFuel reactions elimOrder n = answer
  where
    initNeeds = M.singleton "FUEL" n
    m = foldl (\demand c -> (elim reactions c demand)) initNeeds elimOrder
    [("ORE", answer)] = M.toList m

instance Solution Day14 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (consumeOrDie reactionP) . lines <$> getInputS
    {-
      The reason that we could arrive at a suboptimal solution is because
      that some reactions would introduce extra amount of output chemical
      that we don't use - to minimize that, should work backwards from
      1 FUEL, towards ORE. This maximizes the chance that one output chemical
      can participate in creation other chemicals so to reduce waste.

      If we form a graph by drawing edges from output chemical to the list
      of its input chemicals, we should have a DAG that we can run
      topological sort on - then we can work from 1 FUEL to eliminate
      chemicals following the topological order. (see `elim` function)
     -}
    let reactions :: ReactionTable
        reactions = M.fromListWith (error "expect no duplicated keys") do
          (inChems, (outChem, v)) <- xs
          pure (outChem, (inChems, v))
        topoPrep :: M.Map Chem [Chem]
        topoPrep =
          -- ORE can never appear on RHS.
          M.insert "ORE" [] $
            M.fromList do
              (inChems, (outChem, _)) <- xs
              pure (outChem, M.keys inChems)
        elimOrder = init (topologicalSort topoPrep)
        solve = solveFuel reactions elimOrder
        answer1 = solve 1
    answerShow answer1
    let oneTri = 1000000000000
        low = floor @Double @Int $ ((/) `on` fromIntegral) oneTri answer1
        high : _ =
          -- too lazy to mathmatically work out a upper bound.
          dropWhile ((<= oneTri) . solve) $ iterate (* 2) low
        binarySearch l r =
          -- INVARIANT: `l` is within budget but `r` is not.
          case compare v oneTri of
            LT -> if l == mid then l else binarySearch mid r
            EQ -> mid
            GT -> if r == mid then l else binarySearch l mid
          where
            v = solve mid
            mid = (l + r) `quot` 2
    answerShow (binarySearch low high)
