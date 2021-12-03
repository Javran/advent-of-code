{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2021.Day3
  (
  )
where

import Data.Function
import Data.List
import qualified Data.Map.Strict as M
import Javran.AdventOfCode.Prelude

data Day3

computeFreq :: Ord a => [a] -> [(a, Int)]
computeFreq xs = sortOn snd $
  M.toList $
    M.fromListWith (+) $ do
      x <- xs
      pure (x, 1 :: Int)

chooseBit :: [Bool] -> Bool -> Bool -> Bool
chooseBit xs tiebreak mostCommon = case d of
  [(v, _)] -> v
  [(u, x), (v, y)] -> case compare x y of
    EQ -> tiebreak
    LT -> if mostCommon then v else u
    GT -> if mostCommon then u else v
  _ -> error "Bool can only take 2 values"
  where
    d = M.toList $
      M.fromListWith (+) $ do
        x <- xs
        pure (x, 1 :: Int)

positionalFilter :: Bool -> [[Bool]] -> Int -> [[Bool]]
positionalFilter isOxygen xs i = case transpose xs of
  [] -> xs
  ys ->
    let y = ys !! i
        ft =
          if isOxygen
            then chooseBit y True True
            else chooseBit y False False
     in filter (\x -> x !! i == ft) xs

decodeBinary :: (Foldable t, Num a) => t Bool -> a
decodeBinary = foldl (\acc i -> acc * 2 + if i then 1 else 0) 0

instance Solution Day3 where
  solutionIndex _ = (2021, 3)
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    let tr '0' = False
        tr '1' = True
        tr _ = error "invalid input"
    xs <- (fmap . fmap) tr . lines <$> getInputS
    let ys = transpose xs
        getMostFreq = fst . last . computeFreq
        gamma = fmap getMostFreq ys
        eps = fmap (not . getMostFreq) ys
    answerShow $ ((*) `on` decodeBinary @_ @Int) gamma eps
    let l = length (head xs)
        [oxygenRating] = foldl (\curXs i -> positionalFilter True curXs i) xs [0 .. l -1]
        [co2Rating] = foldl (\curXs i -> positionalFilter False curXs i) xs [0 .. l -1]
    answerShow $ ((*) `on` decodeBinary @_ @Int) oxygenRating co2Rating
