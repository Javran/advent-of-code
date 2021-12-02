{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Javran.AdventOfCode.Y2020.Day13
  (
  )
where

import Data.Bifunctor
import qualified Data.List.Ordered as LOrdered
import Data.List.Split
import Data.Ord
import Javran.AdventOfCode.Prelude
import Math.NumberTheory.GCD

data Day13

-- TODO: newer version of arithmoi removed extendedGCD, find replacement.

mkMults :: Int -> Int -> [(Int, Int)]
mkMults p v = fmap (p,) [z, z + p ..]
  where
    z = (v `div` p) * p

instance Solution Day13 where
  solutionIndex _ = (2020, 13)
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    [rawN, rawStops] <- lines <$> getInputS
    let n :: Int
        n = read rawN
        stopsIndexed :: [(Int, Int)]
        stopsIndexed = fmap (second read) . filter ((/= "x") . snd) $ zip [0 ..] (splitOn "," rawStops)
        stops :: [Int]
        stops = snd <$> stopsIndexed
        merged = foldr1 (LOrdered.unionBy (comparing snd)) $ fmap (\p -> mkMults p n) stops
        (stop, dTime) = head $ dropWhile ((< n) . snd) merged
    answerShow $ stop * (dTime - n)
    -- https://en.wikipedia.org/wiki/Chinese_remainder_theorem#Existence_(direct_construction)
    let prod :: Integer
        prod = product (fmap fromIntegral stops)
        prodAllButOnes :: [Integer]
        prodAllButOnes = fmap (\s -> prod `div` fromIntegral s) stops
        eGcds = zipWith extendedGCD prodAllButOnes (fmap fromIntegral stops)
        ans = sum $ do
          ((ind, p), (bigN, (1, bigM, _v))) <- zip stopsIndexed (zip prodAllButOnes eGcds)
          pure $ fromIntegral ((- ind) `mod` p) * bigN * bigM
    answerShow (ans `mod` prod)
