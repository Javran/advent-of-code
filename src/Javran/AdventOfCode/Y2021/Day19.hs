{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2021.Day19
  (
  )
where

import Control.Monad
import Data.Bifunctor
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Semigroup
import qualified Data.Set as S
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Linear.Affine
import Linear.V3
import Linear.Vector
import Text.ParserCombinators.ReadP hiding (count, many)

data Day19 deriving (Generic)

type Pos = Point V3 Int

type PosSet = S.Set Pos

type ScannerInfo = (Int, [Pos])

scannerHeaderP :: ReadP Int
scannerHeaderP = string "--- scanner " *> decimal1P <* string " ---"

parseScannerInfo :: [String] -> ScannerInfo
parseScannerInfo = \case
  [] -> errInvalid
  (hd : tl) ->
    ( consumeOrDie scannerHeaderP hd
    , fmap
        ((\[a, b, c] -> P (V3 a b c))
           . fmap (read @Int)
           . splitOn ",")
        tl
    )

{-
  Beacon coordinates for a single scanner,
  together with some auxiliary info to help finding scanners that have beacons in common.
 -}
data BeaconAuxSet = BeaconAuxSet
  { basItems :: S.Set Pos
  , basQds :: M.Map Int Int -- the bag of all quadrance of the differences within this set.
  }
  deriving (Show)

mkBeaconAuxSet :: [Pos] -> BeaconAuxSet
mkBeaconAuxSet xs =
  BeaconAuxSet
    { basItems = S.fromList xs
    , basQds
    }
  where
    basQds = M.fromListWith (+) do
      (a, as) <- pickInOrder xs
      (b, _) <- pickInOrder as
      pure (qdA a b, 1)

orientPos :: Pos -> [Pos]
orientPos (P (V3 a b c)) = do
  [x, y, z] <- permutations [a, b, c]
  [sigX, sigY, sigZ] <- replicateM 3 [1, -1]
  pure (P (V3 (sigX * x) (sigY * y) (sigZ * z)))

{-
  Assumes that lhs and rhs have at least 12 beacons in common,
  re-orientation rhs's x,y,z axis so that it matches with that of lhs.
  In addition returns a vector that can translate rhs coords to lhs coords.
 -}
realignRhs :: PosSet -> PosSet -> (PosSet, V3 Int)
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

{-
  All PosSet within MergeResult has their x,y,z axis consistently aligned.
  The accompanying vector translates that PosSet to scanner 0's coordinate system.
  Scanner 0 always has its translation vector being 0.
 -}
type MergeResult = IM.IntMap (PosSet, V3 Int)

{-
  Merges in beacons from scanner r by realigning it using scanner l
  (which must be a member of MergeResult).
 -}
performMerge :: MergeResult -> Int -> Int -> PosSet -> MergeResult
performMerge mr lScannerId rScannerId rSet =
  IM.insert rScannerId (rSet', trR ^+^ trL) mr
  where
    (lhs, trL) = mr IM.! lScannerId
    (rSet', trR) = realignRhs lhs rSet

instance Solution Day19 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap parseScannerInfo . splitOn [""] . lines <$> getInputS
    let ys :: IM.IntMap BeaconAuxSet
        ys = IM.fromList $ (fmap . second) mkBeaconAuxSet xs
        mergablePairs = do
          -- find pairs of scanner ids that share at least 12 beacons
          ((x, xm), as) <- pickInOrder (IM.toList ys)
          ((y, ym), _) <- pickInOrder as
          let qdInCommon =
                -- bag intersection.
                sum $ M.elems (M.intersectionWith min (basQds xm) (basQds ym))
          guard $ qdInCommon >= (12 * 11) `quot` 2
          [(x, y), (y, x)]
        computeMergeResult mr = case scannerToMerge of
          [] -> mr
          (i, j) : _ ->
            computeMergeResult (performMerge mr i j (basItems $ ys IM.! j))
          where
            missingScanners = IS.difference (IM.keysSet ys) (IM.keysSet mr)
            scannerToMerge = do
              sId <- IS.toList missingScanners
              p@(i, j) <- mergablePairs
              p <$ guard (j == sId && IM.member i mr)
    let merged =
          computeMergeResult $
            IM.singleton 0 (basItems (ys IM.! 0), 0) -- scanner 0 as merge base.
        allBeacons = S.unions $ fmap (\(s, tr) -> S.map (.+^ tr) s) $ IM.elems merged
        scannersLoc :: [Pos]
        scannersLoc =
          fmap
            (\(_, tr) ->
               origin .+^ tr)
            $ IM.elems merged
        Just (Max maxDist) = mconcat do
          (p0, as) <- pickInOrder scannersLoc
          (p1, _) <- pickInOrder as
          let (V3 dx dy dz) = p0 .-. p1
          pure . Just . Max . sum $ fmap abs [dx, dy, dz]
    answerShow $ S.size allBeacons
    answerShow maxDist
