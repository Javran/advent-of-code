{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2021.Day15
  (
  )
where

import Control.Monad
import qualified Data.Array.Base as Arr
import qualified Data.Array.ST as Arr
import Data.Bifunctor
import Data.Char
import Data.Function
import Data.Monoid
import qualified Data.Sequence as Seq
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude

data Day15 deriving (Generic)

charToInt :: Char -> Int
charToInt c = ord c - ord '0'

shortestPath :: Arr.UArray (Int, Int) Int -> Arr.UArray (Int, Int) Int
shortestPath vs = Arr.runSTUArray do
  let arrBound = Arr.bounds vs
      neighbors coord =
        filter (inRange arrBound) $
          fmap
            ($ coord)
            [first pred, second pred, first succ, second succ]
  dist <- Arr.newArray arrBound maxBound
  Arr.writeArray dist (0, 0) 0
  fix
    (\loop ->
       \case
         Seq.Empty -> pure dist
         (u Seq.:<| q0) -> do
           distU <- Arr.readArray dist u
           performEnqs <- forM (neighbors u) $ \v -> do
             distV <- Arr.readArray dist v
             let distV' = distU + vs Arr.! v
             if distV' < distV
               then Endo (Seq.|> v) <$ Arr.writeArray dist v distV'
               else pure mempty
           loop $ appEndo (mconcat performEnqs) q0)
    (Seq.singleton (0, 0))

instance Solution Day15 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- (fmap . fmap) charToInt . lines <$> getInputS
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
            norm v = let v' = v `rem` 9 in if v' == 0 then 9 else v'
            gen row5 col5 = norm ((vs Arr.! (r', c')) + rowOff + colOff)
              where
                (rowOff, r') = row5 `quotRem` rows
                (colOff, c') = col5 `quotRem` cols
    let solve g = let dist = shortestPath g in dist Arr.! snd (Arr.bounds dist)
    answerShow $ solve vs
    answerShow $ solve vsFivefold
