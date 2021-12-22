{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
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

module Javran.AdventOfCode.Y2021.Day22
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
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import qualified Data.List.Ordered as LOrd
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
import Linear.Affine
import Linear.V3
import Text.ParserCombinators.ReadP hiding (count, many)
import qualified Data.HashTable.ST.Cuckoo as HT
import Control.Monad.ST
import Debug.Trace
import Data.Int

data Day22 deriving (Generic)

type Seg = ((Int32, Int32), (Int32, Int32), (Int32, Int32))

segP :: ReadP (Bool, Seg)
segP = do
  let intP = readS_to_P (reads @Int32)
      rangeP = do
        _ <- satisfy (`elem` "xyz")
        (,) <$> (char '=' *> intP <* string "..") <*> intP
  o <- (True <$ string "on ") <++ (False <$ string "off ")
  [xr, yr, zr] <- rangeP `sepBy1` char ','
  pure $ (o, (xr, yr, zr))

isInside :: Seg -> Point V3 Int32 -> Bool
isInside ((xL, xR), (yL, yR), (zL, zR)) (P (V3 x y z)) =
  xL <= x && x <= xR && yL <= y && y <= yR && zL <= z && z <= zR

setSegInitOnly :: (Bool, Seg) -> S.Set (Point V3 Int32) -> S.Set (Point V3 Int32)
setSegInitOnly (sOn, seg) s =
  if sOn
    then S.union s $ S.fromList do
      [x, y, z] <- replicateM 3 [-50 .. 50]
      let p = P (V3 x y z)
      guard $ isInside seg p
      pure p
    else S.filter (not . isInside seg) s

-- should be in order and never overlap.
data AtomSeg a
  = AtomSing !a
  | AtomClosed !a !a
  deriving (Eq, Show, Generic)

instance Hashable a => Hashable (AtomSeg a)

mkAtomSeg :: Ord a => a -> a -> AtomSeg a
mkAtomSeg l r
  | l == r = AtomSing l
  | l < r = AtomClosed l r
  | otherwise = error $ "not in order."

atomSegToRange = \case
  AtomSing a -> (a, a)
  AtomClosed a b -> (a, b)

instance Ord a => Ord (AtomSeg a) where
  compare u v = case (u, v) of
    (AtomSing a, AtomSing b) -> compare a b
    (AtomSing a, AtomClosed b c) ->
      if
          | a < b -> LT
          | c < a -> GT
          | otherwise -> error "should not overlap. AS vs AC"
    (AtomClosed a b, AtomSing c) ->
      if
          | c < a -> GT
          | b < c -> LT
          | otherwise -> error "should not overlap. AC vs AS"
    (AtomClosed a b, AtomClosed c d) ->
      if
          | a == b && c == d -> EQ
          | b < c -> LT
          | d < a -> GT
          | otherwise -> error "should not overlap. AC vs AC"

createAtomSegs :: [Int32] -> V.Vector (AtomSeg Int32)
createAtomSegs xsPre = V.fromList $ LOrd.union (fmap AtomSing sings) intervals
  where
    sings = LOrd.nubSort xsPre
    intervals = concat $ zipWith mMakeSeg sings (tail sings)
      where
        mMakeSeg l r
          | l + 1 == r = []
          | otherwise = [mkAtomSeg (l + 1) (r -1)]

-- breakIntoAtomSegs :: V.Vector (AtomSeg Int) -> (Int, Int) -> [AtomSeg Int]
breakIntoAtomSegs segs (lo, hi) = findStart 0 (V.length segs -1)
  where
    findStart l r =
      -- assuming lo must be somewhere
      case compare lo (fst $ atomSegToRange (segs V.! mid)) of
        LT -> findStart l (mid -1)
        EQ -> takeWhile ((<= hi) . snd . atomSegToRange) $ V.toList (V.drop mid segs)
        GT -> findStart (mid + 1) r
      where
        mid = (l + r) `quot` 2

instance Solution Day22 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (consumeOrDie segP) . lines <$> getInputS
    let result = foldl' (flip setSegInitOnly) S.empty xs
        runPart1 = False
    when runPart1 do
      answerShow (S.size result)
    let xSegs = createAtomSegs $ concatMap (\(_, ((lo, hi), _, _)) -> [lo, hi]) xs
        ySegs = createAtomSegs $ concatMap (\(_, (_, (lo, hi), _)) -> [lo, hi]) xs
        zSegs = createAtomSegs $ concatMap (\(_, (_, _, (lo, hi))) -> [lo, hi]) xs

        explode (v, (xRange, yRange, zRange)) = do
          xSeg <- breakIntoAtomSegs xSegs xRange
          ySeg <- breakIntoAtomSegs ySegs yRange
          zSeg <- breakIntoAtomSegs zSegs zRange
          pure ((xSeg, ySeg, zSeg), v)

        area (xSeg, ySeg, zSeg) = f xSeg * f ySeg * f zSeg
          where
            f seg = r - l + 1
              where
                (l, r) = atomSegToRange seg
        result2 = sum $ fmap area $ HM.keys $ HM.filter id $ HM.unions (reverse (fmap (HM.fromList . explode) xs))
        result3 = runST do
          tbl <- HT.new
          forM_ (concatMap (\(i, v) -> traceShow i $ explode v) $ zip [0 ::Int  ..] xs) $ \(k, v) -> do
            HT.insert tbl k v
          let go acc (k, v) = pure $ if v
                then acc + area k
                else acc
          HT.foldM go 0 tbl

    print result3
