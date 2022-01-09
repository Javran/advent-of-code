{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2018.Day6
  (
  )
where

import Control.Monad
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day6 deriving (Generic)

type Coord = (Int, Int)

coordP :: ReadP Coord
coordP = (,) <$> decimal1P <*> (string ", " *> decimal1P)

instance Solution Day6 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (extraOps, rawInput) <- consumeExtraLeadingLines <$> getInputS
    let xs = zip [0 :: Int ..] . fmap (consumeOrDie coordP) . lines $ rawInput
        Just (MinMax2D ((xMin, xMax), (yMin, yMax))) =
          foldMap (Just . minMax2D . snd) xs
        distsToAll = do
          x <- [xMin .. xMax]
          y <- [yMin .. yMax]
          let coord = (x, y)
          pure ((x, y), fmap (second (manhattan coord)) xs)

        coordShortestDists =
          -- k: in-bound coords, v: xId of shortest distance.
          M.fromList do
            (coord, dists) <- distsToAll
            let shortest = minimum (fmap snd dists)
                onlyShortest = filter ((== shortest) . snd) dists
            [t] <- pure onlyShortest
            pure (coord, t)
        excludeXids = IS.fromList do
          coord <-
            [(x, y) | x <- [xMin, xMax], y <- [yMin .. yMax]]
              <> [(x, y) | y <- [yMin, yMax], x <- [xMin .. xMax]]
          Just (xId, _) <- pure (coordShortestDists M.!? coord)
          pure xId
    answerShow $
      maximum $
        IM.elems $ IM.fromListWith (+) do
          (_, (xId, _)) <- M.toList coordShortestDists
          guard $ IS.notMember xId excludeXids
          pure (xId, 1 :: Int)
    let lessThanDist = case extraOps of
          Just ex -> read @Int (head ex)
          Nothing -> 10000
    answerShow $ length do
      (_, dists) <- distsToAll
      let totalDist = sum $ fmap snd dists
      guard $ totalDist < lessThanDist
