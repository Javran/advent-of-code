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

module Javran.AdventOfCode.Y2017.Day25
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

data Day25 deriving (Generic)

newtype MState = MState Int deriving (Eq, Ord)

instance Show MState where
  show (MState v) = "MState " <> [chr (ord 'A' + v)]

data Move = ML | MR deriving (Show)

type WriteMoveTransit = (Bool, Move, MState)

type Rules = Arr.Array (Int, Int) WriteMoveTransit

type Input = ((MState, Int), Rules)

inputP :: ReadP Input
inputP = do
  let nl = void (char '\n')
      dotNl = void (string ".\n")
      valP = (False <$ char '0') <++ (True <$ char '1')
      moveP = (ML <$ string "left") <++ (MR <$ string "right")
      stateP =
        string "state " *> do
          ch <- satisfy isAsciiUpper
          pure $ MState $ ord ch - ord 'A'
      stateDescP = do
        s0 <- between (string "In ") (string ":\n") stateP
        ~[c0, c1] <- forM ["0", "1"] \condV -> do
          _ <- string $ "  If the current value is " <> condV <> ":\n"
          w <- between (string "    - Write the value ") dotNl valP
          m <- between (string "    - Move one slot to the ") dotNl moveP
          t <- between (string "    - Continue with ") dotNl stateP
          pure (w, m, t)
        pure (s0, (c0, c1))
  v0 <-
    (,)
      <$> between (string "Begin in ") (string ".\n") stateP
        <*> between (string "Perform a diagnostic checksum after ") (string " steps.\n") decimal1P
  parsedRules <- many1 (nl *> stateDescP)
  let Just (Max (MState maxMs)) = foldMap (Just . Max . fst) parsedRules
      rules = Arr.array ((0, 0), (maxMs, 1)) do
        (MState lhs, (c0, c1)) <- parsedRules
        [((lhs, 0), c0), ((lhs, 1), c1)]
  pure (v0, rules)

instance Solution Day25 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- consumeOrDie inputP <$> getInputS
    print xs
