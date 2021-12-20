{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2021.Day15
  (
  )
where

import Control.Monad
import qualified Data.Array.Base as Arr
import qualified Data.Array.ST as Arr
import Data.Function
import Data.Monoid
import qualified Data.PSQueue as PQ
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude

data Day15 deriving (Generic)

-- Ref: https://en.wikipedia.org/wiki/Shortest_Path_Faster_Algorithm
shortestPath :: Arr.UArray (Int, Int) Int -> Arr.UArray (Int, Int) Int
shortestPath vs = Arr.runSTUArray do
  let arrBound = Arr.bounds vs
      neighbors coord =
        filter (inRange arrBound) $
          udlrOfCoord coord
  dist <- Arr.newArray arrBound maxBound
  Arr.writeArray dist (0, 0) 0
  fix
    (\loop q ->
       case PQ.minView q of
         Nothing -> pure dist
         Just (u PQ.:-> distU, q0) -> do
           performEnqs <- forM (neighbors u) $ \v -> do
             distV <- Arr.readArray dist v
             let distV' = distU + vs Arr.! v
             if distV' < distV
               then
                 PQ.alter
                   (\case
                      Nothing -> Just distV'
                      Just v' -> Just (min v' distV'))
                   v
                   <$ Arr.writeArray dist v distV'
               else pure id
           loop $ appEndo (foldMap Endo performEnqs) q0)
    (PQ.singleton (0, 0) 0)

instance Solution Day15 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- (fmap . fmap) chToInt . lines <$> getInputS
    let vs = Arr.array ((0, 0), (rows -1, cols -1)) do
          (r, rs) <- zip [0 ..] xs
          (c, x) <- zip [0 ..] rs
          pure ((r, c), x)
        rows = length xs
        cols = length (head xs)
        fivefoldBounds = ((0, 0), (rows * 5 -1, cols * 5 -1))
        vsFivefold = Arr.array fivefoldBounds do
          let ((rLo, cLo), (rHi, cHi)) = fivefoldBounds
          row5 <- [rLo .. rHi]
          col5 <- [cLo .. cHi]
          pure ((row5, col5), gen row5 col5)
          where
            norm v =
              -- modulo 9 gives codomain [0..8], we just need 0 to be 9.
              let v' = v `rem` 9 in if v' == 0 then 9 else v'
            gen row5 col5 = norm ((vs Arr.! (r', c')) + rowOff + colOff)
              where
                (rowOff, r') = row5 `quotRem` rows
                (colOff, c') = col5 `quotRem` cols
    let solve g = let dist = shortestPath g in dist Arr.! snd (Arr.bounds dist)
    answerShow $ solve vs
    answerShow $ solve vsFivefold
