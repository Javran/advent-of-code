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
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2019.Day7
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
import qualified Data.Vector.Unboxed as VU
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2019.IntCode
import Text.ParserCombinators.ReadP hiding (count, many)

data Day7 deriving (Generic)

pipeProgram
  :: IO (Result (VU.Vector Int))
  -> IO (Result (VU.Vector Int))
  -> IO (Result (VU.Vector Int))
pipeProgram lProg rProg = do
  r <- rProg
  case r of
    Done v -> pure (Done v)
    NeedInput rK -> do
      l <- lProg
      case l of
        Done _ -> error "input exhausted"
        NeedInput lK ->
          pure $
            NeedInput $ \input ->
              pipeProgram (lK input) (pure (NeedInput rK))
        SentOutput lOut lK ->
          pipeProgram lK (rK lOut)
    SentOutput o rK -> pure $ SentOutput o (pipeProgram lProg rK)

instance Solution Day7 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (extraOps, rawInput) <- consumeExtraLeadingLines <$> getInputS
    let xs = fmap (read @Int) . splitOn "," . head . lines $ rawInput
        mem = VU.fromList xs
        runPart1 = maybe True ("part1" `elem`) extraOps
        runPart2 = maybe True ("part2" `elem`) extraOps
    when
      runPart1
      do
        signals <- forM (permutations [0 .. 4]) $ \config -> do
          progs <- forM config $ \c -> do
            r <- startProgram mem
            case r of
              NeedInput k ->
                pure $ k c
              _ -> error "unexpected"
          let pipeline = foldl1 pipeProgram progs
          r0 <- pipeline
          case r0 of
            NeedInput k -> do
              r1 <- k 0
              case r1 of
                SentOutput o _ -> pure o
                _ -> error "unexpected"
            _ -> error "unexpected"
        answerShow $ maximum signals
    when
      runPart2
      do
        signals <- forM (permutations [5 .. 9]) $ \config -> do
          progs <- forM config $ \c -> do
            r <- startProgram mem
            case r of
              NeedInput k ->
                pure $ k c
              _ -> error "unexpected"
          let pipeline = foldl1 pipeProgram progs
          Just result <-
            fix
              (\loopback curProg nextInput lastOut -> do
                 r0 <- curProg
                 case r0 of
                   NeedInput k0 -> loopback (k0 $ fromJust nextInput) Nothing lastOut
                   SentOutput o k1 -> loopback k1 (Just o) (Just o)
                   Done {} -> pure lastOut)
              pipeline
              (Just 0)
              Nothing
          pure result
        answerShow $ maximum signals
