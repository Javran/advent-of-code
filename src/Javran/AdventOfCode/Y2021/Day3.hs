{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2021.Day3
  (
  )
where

{- HLINT ignore "Unused LANGUAGE pragma" -}

import Control.Monad
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP

data Day3

getMostFreq xs = fst $ head $ sortOn (negate . snd) d
  where
    d = M.toList $
      M.fromListWith (+) $ do
        x <- xs
        pure (x, 1)

computeFreq :: [Bool] -> Bool -> Bool -> Bool
computeFreq xs tiebreak mostCommon = case d of
  [(v, _)] -> v
  [(u, x), (v, y)] -> case compare x y of
    EQ -> tiebreak
    LT -> if mostCommon then v else u
    GT -> if mostCommon then u else v
  where
    d = M.toList $
      M.fromListWith (+) $ do
        x <- xs
        pure (x, 1)

progress oxygen xs i = case transpose xs of
  [] -> xs
  ys ->
    let y = ys !! i
        ft = if oxygen then computeFreq y True True else computeFreq y False False
     in filter (\x -> x !! i == ft) xs

instance Solution Day3 where
  solutionIndex _ = (2021, 3)
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    let tr '0' = False
        tr '1' = True
    xs <- (fmap . fmap) tr . lines <$> getInputS
    let ys = transpose xs
        t = foldl (\acc i -> acc * 2 + if i then 1 :: Int else 0) 0 $ fmap getMostFreq ys
        t2 = foldl (\acc i -> acc * 2 + if i then 0 else 1) 0 $ fmap getMostFreq ys
        bin = foldl (\acc i -> acc * 2 + if i then 1 :: Int else 0) 0
    answerShow $ t * t2
    let l = length (head xs)
        [oxygenRating] = foldl (\curXs i -> progress True curXs i) xs [0 .. l -1]
        [co2Rating] = foldl (\curXs i -> progress False curXs i) xs [0 .. l -1]
    answerShow $ bin oxygenRating * bin co2Rating
