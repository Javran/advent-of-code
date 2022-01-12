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

instance Solution Day7 where
  solutionSolved _ = False
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
        v ~> ith = graph Arr.! v !! ith
        infixl 4 ~>
    answerS (let (_, n, _) = nodeFromVertex $ head $ topSort graph in n)
    print (childrenWeights rootV)
    print (childrenWeights $ rootV ~> 2)
    print (childrenWeights $ rootV ~> 2 ~> 4)
    print (childrenWeights $ rootV ~> 2 ~> 4 ~> 2)
    let problematic = rootV ~> 2 ~> 4 ~> 2
    -- 5 units heavy.
    answerShow (let (w, _, _) = nodeFromVertex problematic in w - 5)
