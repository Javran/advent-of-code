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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2017.Day7
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import qualified Data.Array as Arr
import Data.Char
import Data.Function
import Data.Function.Memoize (memoFix)
import Data.Graph
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

data Day7 deriving (Generic)

type Prog = ((String, Int), [String])

progP :: ReadP Prog
progP = do
  let nameP = munch1 isAlpha
  pg <- nameP <* string " ("
  w <- decimal1P <* char ')'
  xs <-
    (string " -> " *> (nameP `sepBy1` string ", "))
      <++ (pure [])
  pure ((pg, w), xs)

{-
  Finds the unique value, returns its index and diffs against others.

  would throw if assumptions are broken.
 -}
findImbalance :: forall e. e ~ Int => [(Int, e)] -> Maybe (Int, e)
findImbalance xs = case groupped of
  [_] -> Nothing
  [ls, rs] ->
    -- N.B. element's nonemptiness is irrefutable.
    case (ls, rs) of
      ([(i, x)], ~((_, y) : _)) ->
        -- unique val on left
        Just (i, y - x)
      (~((_, y) : _), [(i, x)]) ->
        -- unique val on right
        Just (i, y - x)
      _ -> error "no unique value present."
  _ -> error "supposed to have 1 or 2 distinct values."
  where
    groupped = groupBy ((==) `on` snd) $ sortOn snd xs

instance Solution Day7 where
  solutionRun _ SolutionContext {getInputS, answerShow, answerS} = do
    xs <- fmap (consumeOrDie progP) . lines <$> getInputS
    let (graph, nodeFromVertex, vertexFromKey) = graphFromEdges do
          ((p, w), qs) <- xs
          pure (w, p, qs)
        getWeight :: Vertex -> Int
        getWeight = memoFix \q v ->
          let (w, _, vs) = nodeFromVertex v in w + sum (fmap (q . fromJust . vertexFromKey) vs)
        rootV = head $ topSort graph
        childrenWeights v = (fmap (getWeight . fromJust . vertexFromKey) vs)
          where
            (_, _, vs) = nodeFromVertex v
        findWrongWeight v wDiff = case findImbalance $ zip [0 ..] (childrenWeights v) of
          Nothing -> let (w, _, _) = nodeFromVertex v in w + wDiff
          Just (ind, wDiff') ->
            findWrongWeight (graph Arr.! v !! ind) wDiff'

    answerS (let (_, n, _) = nodeFromVertex $ head $ topSort graph in n)
    answerShow $ findWrongWeight rootV (error "no imbalances found")
