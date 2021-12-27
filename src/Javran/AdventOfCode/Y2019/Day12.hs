{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2019.Day12
  (
  )
where

import Control.Lens
import qualified Data.Map.Strict as M
import Data.Monoid
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.NumberTheory
import Linear.Affine
import Linear.V3
import Text.ParserCombinators.ReadP hiding (count, many)

data Day12 deriving (Generic)

moonLocationP :: ReadP (Point V3 Int)
moonLocationP = do
  let intP = readS_to_P (reads @Int)
  _ <- string "<x="
  x <- intP
  _ <- string ", y="
  y <- intP
  _ <- string ", z="
  z <- intP
  _ <- string ">"
  pure $ P (V3 x y z)

type Moon = (Point V3 Int {- pos -}, V3 Int {- vel -})

type System = [Moon]

moonEnerge :: Moon -> Int
moonEnerge (P (V3 x y z), V3 vx vy vz) =
  sum (fmap abs [x, y, z]) * sum (fmap abs [vx, vy, vz])

systemEnerge :: System -> Int
systemEnerge = sum . fmap moonEnerge

type OneDim = (Int, Int)

stepOneDim :: [OneDim] -> [OneDim]
stepOneDim xs0 = zip locs' vels'
  where
    (locs, vels) = unzip xs0
    vels' = zipWith (+) vels gravity
    locs' = zipWith (+) locs vels'
    gravity :: [Int]
    gravity = appEndo (foldMap Endo gravityMods) $ replicate (length xs0) 0
      where
        gravityMods = do
          -- pairwise context, indexed by i and j.
          ((i :: Int, (locA, _)), xs1) <- pickInOrder (zip [0 ..] xs0)
          ((j, (locB, _)), _) <- pickInOrder xs1
          pure $ case compare locA locB of
            LT -> (& element i %~ succ) . (& element j %~ pred)
            EQ -> id -- do nothing
            GT -> (& element i %~ pred) . (& element j %~ succ)

unzipSystem :: [Moon] -> ([OneDim], [OneDim], [OneDim])
unzipSystem moons = (xs, ys, zs)
  where
    xs = fmap (\(p, v) -> (p ^. _x, v ^. _x)) moons
    ys = fmap (\(p, v) -> (p ^. _y, v ^. _y)) moons
    zs = fmap (\(p, v) -> (p ^. _z, v ^. _z)) moons

stepSystem :: System -> System
stepSystem moons = zipWith3 combine xs' ys' zs'
  where
    combine (px, vx) (py, vy) (pz, vz) =
      (P (V3 px py pz), V3 vx vy vz)
    (xs, ys, zs) = unzipSystem moons
    xs' = stepOneDim xs
    ys' = stepOneDim ys
    zs' = stepOneDim zs

detectLoop :: Ord a => [a] -> (Int, Int)
detectLoop xs = detectLoopAux traced M.empty
  where
    traced = zip [0 ..] xs

    detectLoopAux ((j, y) : ys) seen = case seen M.!? y of
      Just i -> (i, j)
      Nothing -> detectLoopAux ys (M.insert y j seen)
    detectLoopAux [] _ = error "infinite list expected"

instance Solution Day12 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (extraOps, rawInput) <- consumeExtraLeadingLines <$> getInputS
    let moons = fmap (consumeOrDie moonLocationP) . lines $ rawInput
        initSys :: System
        initSys = zip moons (repeat 0)
        steps = case extraOps of
          Just xs -> read (head xs)
          Nothing -> 1000
    answerShow $ systemEnerge (iterate stepSystem initSys !! steps)
    do
      let answer = chineseRemainder do
            let (xs, ys, zs) = unzipSystem initSys
            oneDimSys <- [xs, ys, zs]
            let (r, p) = detectLoop (iterate stepOneDim oneDimSys)
            pure $ fromIntegral r `modulo` fromIntegral p
      answerShow $ case answer of
        Just (SomeMod m) -> getMod m
        _ -> unreachable
