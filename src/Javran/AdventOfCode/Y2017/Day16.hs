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

module Javran.AdventOfCode.Y2017.Day16
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Coerce
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
import Data.Word
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.TestExtra (consumeExtra)
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day16 deriving (Generic)

newtype Prog = Prog Int deriving (Show)

data Move
  = Spin Int
  | Exchange Int Int
  | Partner Prog Prog
  deriving (Show)

moveP :: ReadP Move
moveP = spinP <++ exchangeP <++ partnerP
  where
    spinP = char 's' *> (Spin <$> decimal1P)
    exchangeP = char 'x' *> (Exchange <$> (decimal1P <* char '/') <*> decimal1P)
    partnerP = char 'p' *> (Partner <$> (progP <* char '/') <*> progP)
    progP :: ReadP Prog
    progP = do
      v <- satisfy (\ch -> ch >= 'a' && ch <= 'p')
      pure $ Prog $ fromIntegral $ ord v - ord 'a'

data Programs = Programs
  { pgToInd :: IM.IntMap Int
  , indToPg :: IM.IntMap Prog
  } deriving Show

mkPrograms :: Int -> Programs
mkPrograms n = Programs {pgToInd = m, indToPg = coerce m}
  where
    m = IM.fromDistinctAscList $ (fmap (\i -> (i, i))) [0 .. n -1]

spin :: Int -> Int -> Programs -> Programs
spin n dv Programs {pgToInd} = Programs {pgToInd = pgToInd', indToPg = coerce indToPg'}
  where
    pgToInd' = fmap (\v -> (v + dv) `rem` n) pgToInd
    indToPg' = IM.fromList . fmap swap $ IM.toList pgToInd'

exchange :: Int -> Int -> Programs -> Programs
exchange i j Programs {pgToInd, indToPg} = Programs {pgToInd = pgToInd', indToPg = indToPg'}
  where
    pI@(Prog i') = indToPg IM.! i
    pJ@(Prog j') = indToPg IM.! j
    indToPg' = IM.insert i pJ . IM.insert j pI $ indToPg
    pgToInd' = IM.insert j' i . IM.insert i' j $ pgToInd

partner :: Prog -> Prog -> Programs -> Programs
partner pI@(Prog i) pJ@(Prog j) Programs {pgToInd, indToPg} =
  Programs {pgToInd = pgToInd', indToPg = indToPg'}
  where
    i' = pgToInd IM.! i
    j' = pgToInd IM.! j
    pgToInd' = IM.insert i j' . IM.insert j i' $ pgToInd
    indToPg' = IM.insert j' pI . IM.insert i' pJ $ indToPg

applyMove n = \case
  Spin v -> spin n v
  Exchange i j -> exchange i j
  Partner i j -> partner i j

ppr Programs {indToPg} = fmap render $ IM.elems indToPg
  where
    render (Prog v) = chr $ ord 'a' + v

instance Solution Day16 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerS} = do
    (extraOps, rawInput) <- consumeExtra getInputS
    let xs = consumeOrDie ((moveP `sepBy` char ',') <* char '\n') $ rawInput
        n = case extraOps of
          Just ~[rawN] -> read @Int rawN
          Nothing -> 16
        initSt = mkPrograms n
        finSt = foldl' (\cur move -> applyMove n move cur) initSt xs
    answerS (ppr finSt)

