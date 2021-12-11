{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Javran.AdventOfCode.Y2021.Day11
  (
  )
where

import Control.Monad
import Data.Bifunctor
import Data.Char
import qualified Data.DList as DL
import Data.List
import qualified Data.Map.Merge.Strict as M
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude

data Day11 deriving (Generic)

chToInt :: Char -> Int
chToInt ch = ord ch - ord '0'

type Coord = (Int, Int)

type OctoMap = M.Map Coord Int

adjacents :: Coord -> [Coord]
adjacents c@(x, y) = do
  x' <- [x -1 .. x + 1]
  y' <- [y -1 .. y + 1]
  let c' = (x', y')
  guard $ c' /= c
  pure c'

{-
  FlashingOverlay = (s, adj)

  where s is the set of octopus that is flashing.

  adj is a cache that should be equal to

  > adj = M.fromListWith (+) do
  >   coord <- S.toList s
  >   coord' <- adjacents coord
  >   pure (coord', 1 :: Int)

  Note that invalid coords are allowed in this mapping.

  This cache is an optimization so we don't have to compute
  adj at each step, but one must be careful that incremental updates of
  adj must be performed correctly.

 -}
type FlashingOverlay =
  ( S.Set Coord
  , M.Map Coord Int
  )

-- keeps expanding the set of flashing octopus until fixpoint.
flashingSet :: OctoMap -> FlashingOverlay -> Maybe FlashingOverlay
flashingSet m (flashing, adjFlashes) =
  if S.null newlyFlashing
    then Nothing
    else Just (S.union flashing newlyFlashing, adjFlashes')
  where
    adjFlashes' = M.unionWith (+) adjFlashes $ M.fromListWith (+) do
      c <- S.toList newlyFlashing
      c' <- adjacents c
      guard $ M.member c' m
      pure (c', 1)
    newlyFlashing =
      M.keysSet $
        M.filterWithKey
          (\coord v ->
             v + fromMaybe 0 (adjFlashes M.!? coord) > 9)
          $ M.withoutKeys m flashing

flashAux :: OctoMap -> FlashingOverlay -> OctoMap
flashAux m overlay@(_, adjFlashes) =
  case flashingSet m overlay of
    Just overlay' ->
      flashAux m overlay'
    Nothing ->
      M.merge
        M.preserveMissing
        M.dropMissing
        (M.zipWithMatched (const (+)))
        m
        adjFlashes

flash :: OctoMap -> (S.Set Coord, OctoMap)
flash m = (S.fromDistinctAscList $ DL.toList s, m'')
  where
    -- resets flashing octopus and accumulates those octopus into a set of coords.
    (s, m'') = M.mapAccumWithKey go DL.empty m'
      where
        go xs k v =
          if v > 9
            then (xs <> DL.singleton k, 0)
            else (xs, v)
    m' = flashAux m mempty

step :: OctoMap -> (Int, OctoMap)
step = first S.size . flash . M.map succ

instance Solution Day11 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (fmap chToInt) . lines <$> getInputS
    let mInit :: OctoMap
        mInit = M.fromList do
          (r, row) <- zip [0 ..] xs
          (c, x) <- zip [0 ..] row
          pure ((r, c), x)
    let progression =
          unfoldr
            (\curM ->
               let w@(_, m') = step curM
                in Just (w, m'))
            mInit
    answerShow $ sum $ fmap fst $ take 100 progression
    let (ans :: Int, _) : _ =
          dropWhile (not . all (== 0) . snd . snd) $ zip [1 ..] progression
    answerShow ans
