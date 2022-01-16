{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2017.Day21
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Char
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Proxy
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import GHC.Generics (Generic)
import GHC.TypeNats
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day21 deriving (Generic)

data Grid (n :: Nat) = Grid Int deriving (Eq, Ord)

instance (KnownNat n, 1 <= n, n <= 5) => Bounded (Grid n) where
  minBound = Grid 0
  maxBound =
    Grid
      let sz = fromIntegral $ natVal (Proxy @n)
       in foldl' setBit 0 [0 .. sz * sz -1]

type Coord = (Int, Int) -- row, col

encodeGrid :: KnownNat n => proxy n -> [[Bool]] -> Grid n
encodeGrid pSz xss =
  if sz * sz == length xs
    then Grid $ foldl' (\acc (i, v) -> if v then setBit acc i else acc) 0 (zip [0 ..] xs)
    else error "unexpected length"
  where
    sz = fromIntegral $ natVal pSz
    xs = concat xss

decodeGrid :: forall n. KnownNat n => Grid n -> [[Bool]]
decodeGrid (Grid v) = chunksOf sz $ fmap (testBit v) [0 .. sz * sz -1]
  where
    sz = fromIntegral $ natVal (Proxy @n)

allTransforms :: Int -> (Coord -> a) -> [Coord -> a]
allTransforms len f0 = do
  let flipVert f (r, c) = f (len -1 - r, c)
      rotateCwQt f (r, c) = f (len -1 - c, r)
  f1 <- [f0, flipVert f0]
  take 4 (iterate rotateCwQt f1)

allTransformsOf :: forall n. KnownNat n => Grid n -> [Grid n]
allTransformsOf g = fmap (encodeGrid (Proxy @n)) reps
  where
    rep = decodeGrid g
    reps :: [] [[Bool]]
    reps = fmap viewerToRep allViewers
    viewer (r, c) = rep !! r !! c
    viewerToRep :: (Coord -> Bool) -> [[Bool]]
    viewerToRep v = do
      r <- [0 .. sz -1]
      pure [v (r, c) | c <- [0 .. sz -1]]
    allViewers = allTransforms sz viewer
    sz :: Int
    sz = fromIntegral $ natVal (Proxy @n)

type ParsedRule n = (Grid n, Grid (n + 1))

gridP :: KnownNat n => proxy n -> ReadP (Grid n)
gridP pSz = do
  r0 <- rowP
  rs <- replicateM (sz -1) (char '/' *> rowP)
  pure $ encodeGrid pSz (r0 : rs)
  where
    rowP = replicateM sz cellP
    cellP = (False <$ char '.') <++ (True <$ char '#')
    sz = fromIntegral $ natVal pSz

ruleP :: forall n proxy. (KnownNat n, KnownNat (n + 1)) => proxy n -> ReadP (ParsedRule n)
ruleP pSz = do
  lhs <- gridP pSz
  _ <- string " => "
  rhs <- gridP (Proxy @(n + 1))
  pure (lhs, rhs)

inputP :: ReadP ([ParsedRule 2], [ParsedRule 3])
inputP = do
  let nl = char '\n'
  rs0 <- many (ruleP (Proxy @2) <* nl)
  rs1 <- many (ruleP (Proxy @3) <* nl)
  pure (rs0, rs1)

renderGrid :: KnownNat n => Grid n -> [String]
renderGrid = (fmap . fmap) (bool '.' '#') . decodeGrid

type RuleTable n = V.Vector (Maybe (Grid (n + 1)))

buildRuleTable :: forall n. (KnownNat n, 1 <= n, n <= 5) => [ParsedRule n] -> RuleTable n
buildRuleTable ps = V.create do
  let Grid mx = maxBound `asTypeOf` (fst $ head ps)
  vec <- VM.replicate (mx + 1) Nothing
  -- first, make sure all explicitly stated rules are there.
  forM_ ps \(Grid lhs, rhs) -> do
    VM.unsafeWrite vec lhs (Just rhs)
  -- then populate derived rules.
  forM_ ps \(l, rhs@(Grid rr)) ->
    forM_ (allTransformsOf l) \(Grid lhs) -> do
      mOldRule <- VM.unsafeRead vec lhs
      case mOldRule of
        Nothing -> VM.unsafeWrite vec lhs (Just rhs)
        Just rhs'@(Grid rr') ->
          -- should be safe to assume overall consistency
          when (rhs' /= rhs) do
            error $ "rule inconsistency: lhs: " <> show lhs <> " rhs: " <> show (rr, rr')
  pure vec

instance Solution Day21 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (xs, ys) <- consumeOrDie inputP <$> getInputS
    let rs0 = buildRuleTable xs
        rs1 = buildRuleTable ys
    print
      (concatMap
         (\case
            Nothing -> []
            Just (Grid v) -> [v])
         rs0)
    print
      (concatMap
         (\case
            Nothing -> []
            Just (Grid v) -> [v])
         rs1)
    print (length rs0)
    print (length rs1)
