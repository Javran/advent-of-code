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
import Control.Lens
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
import Javran.AdventOfCode.TestExtra
import Javran.AdventOfCode.Y2017.Day6 (rotateRightBy)
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day16 deriving (Generic)

newtype Prog = Prog Int deriving (Show, Eq, Ord)

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

{-
  TODO: now that we've done it the stupid way, let's speed this up.
 -}
data Programs = Programs
  { pgToInd :: IM.IntMap Int
  , indToPg :: IM.IntMap Prog
  }
  deriving (Show)

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

ppr2 = fmap render
  where
    render (Prog v) = chr $ ord 'a' + v

findFix :: Ord a => M.Map a Int -> [(Int, a)] -> (Int, Int)
findFix seen ~((j, x) : xs) = case seen M.!? x of
  Just i -> (i, j)
  Nothing -> findFix (M.insert x j seen) xs

type Programs2 = [Prog]

mkPrograms2 :: Int -> Programs2
mkPrograms2 n = coerce [0 .. n -1]

spin2 :: Int -> Int -> Programs2 -> Programs2
spin2 n d = rotateRightBy n d

exchange2 :: Int -> Int -> Programs2 -> Programs2
exchange2 i j xs = xs & ix i .~ pJ & ix j .~ pI
  where
    pI = xs !! i
    pJ = xs !! j

partner2 :: Prog -> Prog -> Programs2 -> Programs2
partner2 pI pJ xs = xs & ix i .~ pJ & ix j .~ pI
  where
    tracedXs = zip [0 ..] xs
    firstMatch :: Prog -> (Int, Prog) -> Alt Maybe Int
    firstMatch needle (ind, x) = Alt $ ind <$ guard (needle == x)
    (Alt (Just i), Alt (Just j)) =
      foldMap (\x' -> (firstMatch pI x', firstMatch pJ x')) tracedXs

applyMove2 n = \case
  Spin v -> spin2 n v
  Exchange i j -> exchange2 i j
  Partner i j -> partner2 i j

{-
  Note for part 2: At first this seems to be just a permutation
  that we can speed up through matrix multiplication, which isn't actually the case.
  In particular, operation `Partner` doesn't do a consistent permutation as what it does
  depends on where those elements are.

  I don't think simulation all the way is gonna fly, so we'll probably looking
  at finding a fixpoint.
 -}

instance Solution Day16 where
  solutionRun _ SolutionContext {getInputS, answerS} = do
    (extraOps, rawInput) <- consumeExtra getInputS
    let xs = consumeOrDie ((moveP `sepBy` char ',') <* char '\n') $ rawInput
        n = case extraOps of
          Just ~[rawN] -> read @Int rawN
          Nothing -> 16
        initSt = mkPrograms n
        applyAll z = foldl' (\cur move -> applyMove n move cur) z xs
        finSt = applyAll initSt
    let s0 = mkPrograms2 n
        applyAll2 z = foldl' (\cur move -> applyMove2 n move cur) z xs
        sFin = applyAll2 s0
    answerS (ppr2 sFin)
    do
      let progression = iterate applyAll2 s0
          (0, period) = findFix mempty (zip [0 ..] $ progression)
          r = 1_000_000_000 `rem` period
      answerS $ ppr2 $ progression !! r
