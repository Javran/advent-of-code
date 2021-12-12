{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2019.Day10
  (
  )
where

import Data.Function
import Data.List
import qualified Data.Map.Strict as M
import Data.Ord
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Linear.Affine
import Linear.V2
import Linear.Vector

data Day10 deriving (Generic)

type Coord = Point V2 Int

instance Solution Day10 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    raws <- lines <$> getInputS
    let asteroids :: [Coord]
        asteroids = do
          (y, rs) <- zip [0 ..] raws
          (x, '#') <- zip [0 ..] rs
          pure $ P $ V2 x y
        stations :: [(Point V2 Int, M.Map (V2 Int) [Coord])]
        stations = do
          (c, others) <- pick asteroids
          pure
            ( c
            , M.fromListWith (<>) do
                cur <- others
                let d10@(V2 dx dy) = cur .-. c
                    slope = (`div` gcd dx dy) <$> d10
                    sign = signum $ perp slope `crossZ` d10
                -- with slope with a direction (as sign) to group other asteroids.
                pure (slope ^* sign, [cur])
            )
        (stationLoc, bestM) = maximumBy (comparing (M.size . snd)) stations
    answerShow $ M.size bestM
    let shiftAngle v =
          -- shift the resulting angle so that we starts with angle pi/2
          if v >= pi / 2 then v else v + pi * 2
        stationDist asteroid =
          -- all points we want to sort are on the same line - Manhattan distance should do.
          abs dx + abs dy
          where
            V2 dx dy = asteroid .-. stationLoc
        ordered =
          fmap (sortOn stationDist . snd) $
            sortOn
              ((\(V2 x y) -> shiftAngle $ (atan2 @Double `on` fromIntegral) y x) . fst)
              $ M.toList bestM
        elimOrders =
          concat $
            unfoldr
              (\cur -> do
                 _ : _ <- pure cur
                 -- elimate heads from each list
                 let (hdsPre, tlsPre) = unzip $ fmap (splitAt 1) cur
                     tls = filter (not . null) tlsPre
                 Just (concat hdsPre, tls))
              ordered
    let (P (V2 x y)) =
          if length elimOrders < 200
            then last elimOrders
            else elimOrders !! (200 - 1)
    answerShow (x * 100 + y)
