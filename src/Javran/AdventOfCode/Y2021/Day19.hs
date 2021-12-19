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

module Javran.AdventOfCode.Y2021.Day19
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
import Debug.Trace
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Linear.Affine
import Linear.V3
import Linear.Vector
import Text.ParserCombinators.ReadP hiding (count, many)

data Day19 deriving (Generic)

type Pos = Point V3 Int

type Beacons = [Pos]

type ScannerInfo = (Int, Beacons)

data BeaconSet = BeaconSet
  { bsItems :: S.Set Pos
  , bsQds :: M.Map Int Int
  }
  deriving (Show)

mkBeaconSet :: Beacons -> BeaconSet
mkBeaconSet xs = BeaconSet {bsItems = S.fromList xs, bsQds}
  where
    bsQds = M.fromListWith (+) do
      (a, as) <- pickInOrder xs
      (b, _) <- pickInOrder as
      pure (qdA a b, 1)

scannerHeaderP :: ReadP Int
scannerHeaderP = string "--- scanner " *> decimal1P <* string " ---"

orientPos :: Pos -> [Pos]
orientPos (P (V3 a b c)) = do
  (x, xs) <- pick [a, b, c]
  (y, ys) <- pick xs
  (z, _) <- pick ys
  sigX <- [1, -1]
  sigY <- [1, -1]
  sigZ <- [1, -1]
  pure (P (V3 (sigX * x) (sigY * y) (sigZ * z)))

parseScannerInfo :: [String] -> ScannerInfo
parseScannerInfo = \case
  [] -> errInvalid
  (hd : tl) -> (consumeOrDie scannerHeaderP hd, fmap ((\[a, b, c] -> P (V3 a b c)) . fmap (read @Int) . splitOn ",") tl)

realignRhs :: S.Set Pos -> S.Set Pos -> (S.Set Pos, V3 Int)
realignRhs lhs rhsPre = head do
  -- try all orientations for rhs
  rhs <- transpose (fmap orientPos (S.toList rhsPre))
  l <- S.toList lhs
  r <- rhs
  let tr = l .-. r
      translated = fmap (.+^ tr) rhs
      rSet = S.fromList translated
  guard $ S.size (S.intersection lhs rSet) >= 12
  pure (S.fromList rhs, tr)

-- aligned positions with a vector to translate back to scanner 0 coordinate system
-- scanner 0 always have the vector being zero
type MergeResult = IM.IntMap (S.Set Pos, V3 Int)

performMerge :: MergeResult -> Int -> Int -> S.Set Pos -> MergeResult
performMerge mr lScannerId rScannerId rSet =
  IM.insert rScannerId (rSet', trR ^+^ trL) mr
  where
    (lhs, trL) = mr IM.! lScannerId
    (rSet', trR) = realignRhs lhs rSet

instance Solution Day19 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap parseScannerInfo . splitOn [""] . lines <$> getInputS
    let ys :: IM.IntMap BeaconSet
        ys = IM.fromList $ (fmap . second) mkBeaconSet xs
        mergablePairs = do
          ((x, xm), as) <- pickInOrder (IM.toList ys)
          ((y, ym), _) <- pickInOrder as
          let qdInCommon = sum $ M.elems (M.intersectionWith (\a b -> min a b) (bsQds xm) (bsQds ym))
          guard $ qdInCommon >= 66
          [(x, y), (y, x)]
        initMergeResult = IM.singleton 0 (bsItems (ys IM.! 0), 0)
        computeMergeResult mr =
          if null scannerToMerge
            then mr
            else
              let (i, j) : _ = scannerToMerge
               in computeMergeResult (performMerge mr i j (bsItems $ ys IM.! j))
          where
            missingScanners = IS.difference (IM.keysSet ys) (IM.keysSet mr)
            scannerToMerge = do
              sId <- IS.toList missingScanners
              (i, j) <- mergablePairs
              guard (j == sId)
              guard (IM.member i mr)
              pure (i, j)

    let merged = computeMergeResult initMergeResult
        allBeacons = S.unions $ fmap (\(s, tr) -> S.map (.+^ tr) s) $ IM.elems merged
        scannersLoc :: [Pos]
        scannersLoc = do
          (_, tr) <- IM.elems merged
          pure (0 .+^ tr)
        Just (Max maxDist) = mconcat do
          ((P (V3 x0 y0 z0)), as) <- pick $ scannersLoc
          ((P (V3 x1 y1 z1), _)) <- pick as
          pure (Just (Max (sum (fmap abs [x0 - x1, y0 - y1, z0 - z1]))))
    answerShow (S.size allBeacons)
    answerShow maxDist
