{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Javran.AdventOfCode.Y2020.Day13
  (
  )
where

import Control.Monad
import Data.Bifunctor
import qualified Data.List.Ordered as LOrdered
import Data.Ord
import Javran.AdventOfCode.Prelude
import Math.NumberTheory.Moduli.Chinese
import Math.NumberTheory.Moduli.Class

data Day13

-- Ref: https://github.com/Bodigrim/arithmoi/issues/69
chineseRemainder :: [SomeMod] -> Maybe SomeMod
chineseRemainder (x : xs) = foldM chineseSomeMod x xs
chineseRemainder _ = Just (0 `modulo` 1)

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
    let answer = chineseRemainder $ do
          (ind, p) <- stopsIndexed
          pure (fromIntegral ((- ind) `mod` p) `modulo` fromIntegral p)
    answerShow $ case answer of
      Just (SomeMod m) -> getVal m
      _ -> undefined
