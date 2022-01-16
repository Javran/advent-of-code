{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Javran.AdventOfCode.Y2017.Day21
  (
  )
where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.List
import Data.List.Split hiding (sepBy)
import Data.Proxy
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import GHC.TypeNats
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.TestExtra
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day21 deriving (Generic)

data Grid (n :: Nat) = Grid Int deriving (Eq, Ord)

type PlainGrid = [[Bool]]

instance (KnownNat n, 1 <= n, n <= 5) => Bounded (Grid n) where
  minBound = Grid 0
  maxBound =
    Grid
      let sz = fromIntegral $ natVal (Proxy @n)
       in foldl' setBit 0 [0 .. sz * sz -1]

type Coord = (Int, Int) -- row, col

encodeGrid :: KnownNat n => proxy n -> PlainGrid -> Grid n
encodeGrid pSz xss =
  if sz * sz == length xs
    then Grid $ foldl' (\acc (i, v) -> if v then setBit acc i else acc) 0 (zip [0 ..] xs)
    else error "unexpected length"
  where
    sz = fromIntegral $ natVal pSz
    xs = concat xss

decodeGrid :: forall n. KnownNat n => Grid n -> PlainGrid
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
    reps :: [] PlainGrid
    reps = fmap viewerToRep allViewers
    viewer (r, c) = rep !! r !! c
    viewerToRep :: (Coord -> Bool) -> PlainGrid
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

type RuleTable n = V.Vector (Maybe (Grid (n + 1)))

buildRuleTable :: forall n. (KnownNat n, 1 <= n, n <= 5) => [ParsedRule n] -> RuleTable n
buildRuleTable ps = V.create do
  let Grid mx = maxBound @(Grid n)
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

type AllRules = (RuleTable 2, RuleTable 3)

toPlainGrid :: forall n. KnownNat n => [[Grid n]] -> PlainGrid
toPlainGrid = concatMap convert
  where
    convert :: [Grid n] -> PlainGrid
    convert = fmap concat . transpose . fmap decodeGrid

type GGrids = Either [[Grid 2]] [[Grid 3]]

fromPlainGrid :: PlainGrid -> GGrids
fromPlainGrid pg
  | even len = Left $ divideGrid (Proxy @2)
  | len `rem` 3 == 0 = Right $ divideGrid (Proxy @3)
  | otherwise = error $ "unexpected length: " <> show len
  where
    divideGrid :: forall n. KnownNat n => Proxy n -> [[Grid n]]
    divideGrid pN =
      fmap
        (fmap (encodeGrid pN)
           . transpose
           . fmap (chunksOf n))
        $ chunksOf n pg
      where
        n = fromIntegral $ natVal pN
    len = length pg

applyRuleTable :: RuleTable n -> Grid n -> Grid (n + 1)
applyRuleTable rt (Grid v) = case rt V.! v of
  Nothing -> error $ "no rule for lhs " <> show v
  Just rhs -> rhs

applyRules :: AllRules -> GGrids -> GGrids
applyRules (rt2, rt3) =
  fromPlainGrid
    . either
      (toPlainGrid . (fmap . fmap) (applyRuleTable rt2))
      (toPlainGrid . (fmap . fmap) (applyRuleTable rt3))

instance Solution Day21 where
  solutionRun _ SolutionContext {getInputS, answerShow, answerS} = do
    (extraOps, rawInput) <- consumeExtra getInputS
    let (pr2, pr3) = consumeOrDie inputP rawInput
        rt2 = buildRuleTable pr2
        rt3 = buildRuleTable pr3
        step = applyRules (rt2, rt3)
        g0 = Right [[consumeOrDie (gridP (Proxy @3)) ".#./..#/###"]]
        progression = iterate step g0
    case extraOps of
      Nothing -> do
        do
          let pg = either toPlainGrid toPlainGrid (progression !! 5)
          answerShow $ countLength id (concat pg)
        do
          let pg = either toPlainGrid toPlainGrid (progression !! 18)
          answerShow $ countLength id (concat pg)
      Just ~[rawN] -> do
        let n = read @Int rawN
        forM_ (zip [0 .. n] progression) \(i, gg) -> do
          let pg = either toPlainGrid toPlainGrid gg
          answerShow i
          forM_ pg \row ->
            answerS (fmap (bool '.' '#') row)
