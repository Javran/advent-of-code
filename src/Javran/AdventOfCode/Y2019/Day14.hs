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
import Data.Bifunctor
import Data.Bool
import Data.Char
import Data.Either
import Data.Foldable
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
import Text.ParserCombinators.ReadP hiding (count, many)
import qualified Data.Graph as G

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

topologicalSort :: forall f a. (Foldable f, Ord a) => M.Map a (f a) -> [a]
topologicalSort g = topoAux initInDegs []
  where
    allK =
      {-
        TODO: this is a quick and dirty way
        to make sure all node exists in initInDegs,
       -}
      M.fromList ((,0) <$> M.keys g)
    initInDegs :: M.Map a Int
    initInDegs = M.unionWith (+) allK $ M.fromListWith (+) do
      (_, vs) <- M.toList g
      v <- toList vs
      pure (v, 1)
    topoAux inDegs acc =
      if null inDegs
        then reverse acc
        else
          let (rmTarget, _) =
                -- not optimal, aiming for correctness for now.
                minimumBy (comparing snd) (M.toList inDegs)
              removes :: [a]
              removes = do
                Just cs <- pure $ g M.!? rmTarget
                toList cs
              inDegs' =
                foldr
                  (\c m ->
                     M.alter
                       (\case
                          Nothing -> Nothing
                          Just v -> do
                            Just (v -1))
                       c
                       m)
                  (M.delete rmTarget inDegs)
                  removes
          in topoAux inDegs' (rmTarget : acc)

elim :: ReactionTable -> Chem -> M.Map Chem Int -> M.Map Chem Int
elim tbl rmTarget m = case m M.!? rmTarget of
  Just needCnt ->
    let (lhs, rhsCnt) = tbl M.! rmTarget
        factor = ceiling @Double @Int $ ((/) `on` fromIntegral) needCnt rhsCnt
    in M.unionWith (+) (M.delete rmTarget m) (M.map (*factor) lhs)
  Nothing -> m

instance Solution Day14 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (consumeOrDie reactionP) . lines <$> getInputS
    {-
      TODO:
      - determine order of elimination by topological sort.
        (if we form graph that has edges from output chem to input chems,
        we should have a DAG.)

      - perform elimination using rewrites following that order.

      - this can be further fused: as soon as the next element in the list of elimination
        is determined, the rewrite can be performed.

     -}
    let reactions :: ReactionTable
        reactions = M.fromListWith (error "expect no duplicated keys") do
          (inChems, (outChem, v)) <- xs
          pure (outChem, (inChems, v))
        topoPrep :: M.Map Chem [Chem]
        topoPrep = M.fromList do
          (inChems, (outChem, _)) <- xs
          pure (outChem, M.keys inChems)
        elimOrder = init $ topologicalSort topoPrep
        initNeeds = M.singleton "FUEL" 1
    print elimOrder
    let elim' = elim reactions
    print (foldl (\curNeed rm -> elim' rm curNeed) initNeeds elimOrder)
