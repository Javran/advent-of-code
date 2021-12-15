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
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude

data Day15 deriving (Generic)

charToInt :: Char -> Int
charToInt c = ord c - ord '0'

shortestPath :: V.Vector (V.Vector Int) -> Arr.UArray (Int, Int) Int
shortestPath vs = Arr.runSTUArray do
  let rows = V.length vs
      cols = V.length (V.head vs)
      arrBound = ((0, 0), (rows -1, cols -1))
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
           performEnqs <- forM (neighbors u) $ \v@(vR, vC) -> do
             distV <- Arr.readArray dist v
             let distV' = distU + vs V.! vR V.! vC
             if distV' < distV
               then Endo (Seq.|> v) <$ Arr.writeArray dist v distV'
               else pure mempty
           loop $ appEndo (mconcat performEnqs) q0)
    (Seq.singleton (0, 0))

instance Solution Day15 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- (fmap . fmap) charToInt . lines <$> getInputS
    let vs :: V.Vector (V.Vector Int)
        vs = V.fromList (V.fromList <$> xs)
        rows = V.length vs
        cols = V.length (V.head vs)
        vsFivefold =
          V.generate (rows * 5) (\row5 -> V.generate (cols * 5) (\col5 -> gen row5 col5))
          where
            norm v = let v' = v `rem` 9 in if v' == 0 then 9 else v'
            gen row5 col5 = norm ((vs V.! r' V.! c') + rowOff + colOff)
              where
                (rowOff, r') = row5 `quotRem` rows
                (colOff, c') = col5 `quotRem` cols
    let ans1 = shortestPath vs
        ans2 = shortestPath vsFivefold
    answerShow $ ans1 Arr.! (rows -1, cols -1)
    answerShow $ ans2 Arr.! (rows * 5 -1, cols * 5 -1)
