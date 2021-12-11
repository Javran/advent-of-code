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

-- keeps expanding the set of flashing octopus until fixpoint.
flashingSet :: OctoMap -> S.Set Coord -> S.Set Coord
flashingSet m alreadyFlashing = M.keysSet m'
  where
    m' =
      M.filterWithKey
        (\coord v ->
           let flashing = v + fromMaybe 0 (adjFlashes M.!? coord) > 9
            in flashing)
        m
    adjFlashes = M.fromListWith (+) do
      coord <- S.toList alreadyFlashing
      coord' <- adjacents coord
      pure (coord', 1 :: Int)

flashAux :: OctoMap -> S.Set Coord -> OctoMap
flashAux m s =
  if S.size s == S.size s'
    then
      let adjFlashes = M.fromListWith (+) do
            coord <- S.toList s
            coord' <- adjacents coord
            pure (coord', 1 :: Int)
       in M.merge
            M.preserveMissing
            M.dropMissing
            (M.zipWithMatched (const (+)))
            m
            adjFlashes
    else flashAux m s'
  where
    s' = flashingSet m s

flash :: OctoMap -> (S.Set Coord, OctoMap)
flash m = (M.keysSet $ M.filter (\v -> v > 9) m', M.map (\v -> if v > 9 then 0 else v) m')
  where
    m' = flashAux m S.empty

step :: OctoMap -> (Int, OctoMap)
step = first S.size . flash . M.map succ

instance Solution Day11 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (fmap chToInt) . lines <$> getInputS
    let m :: OctoMap
        m = M.fromList do
          (r, row) <- zip [0 ..] xs
          (c, x) <- zip [0 ..] row
          pure ((r, c), x)
    let progression =
          unfoldr
            (\curM ->
               let w@(_, m') = step curM
                in Just (w, m'))
            m
    answerShow $ sum $ fmap fst $ take 100 progression
    let (ans :: Int, _) : _ =
          dropWhile (not . all (== 0) . snd . snd) $ zip [1 ..] progression
    answerShow ans
