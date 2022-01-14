{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2017.Day17
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Char
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.STRef
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day17 deriving (Generic)

data Node s = Node
  { nVal :: Int
  , nNext :: STRef s (Node s)
  }

forward :: STRef s (Node s) -> ST s (STRef s (Node s))
forward n = nNext <$> readSTRef n

buildBuffer :: Int -> Int -> ST s (STRef s (Node s), STRef s (Node s))
buildBuffer fwdCnt tillN = do
  sInit <- mdo
    s <- newSTRef $ Node 0 s
    pure s
  let forwardN n =
        if fwdCnt < 0
          then error "negative forward count"
          else (foldl' (>=>) pure $ replicate fwdCnt forward) n
      oneStep n0 val = do
        n1 <- forwardN n0
        Node _ n1Next <- readSTRef n1
        n1Next' <- newSTRef $ Node val n1Next
        modifySTRef' n1 \n -> n {nNext = n1Next'}
        pure n1Next'
  sFinal <- foldM (\cur val -> oneStep cur val) sInit [1 .. tillN]
  pure (sInit, sFinal)

instance Solution Day17 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    n <- read @Int . head . lines <$> getInputS
    answerShow $ runST do
      (_, sFinal) <- buildBuffer n 2017
      Node ans _ <- readSTRef =<< forward sFinal
      pure ans
    answerShow $ runST do
      (sInit, _) <- buildBuffer n 50_000_000
      Node ans _ <- readSTRef =<< forward sInit
      pure ans
