{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Javran.AdventOfCode.Y2015.Day14
  (
  )
where

import Data.Char
import qualified Data.Map.Strict as M
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.TestExtra
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day14 deriving (Generic)

type Reindeer = (String, (Int, Int, Int))

reindeerP :: ReadP Reindeer
reindeerP = do
  n <- munch1 isLetter
  strP " can fly "
  v <- decimal1P
  strP " km/s for "
  tActive <- decimal1P
  strP " seconds, but then must rest for "
  tRest <- decimal1P
  strP " seconds."
  pure (n, (v, tActive, tRest))

travelDist :: Int -> Reindeer -> Int
travelDist t (_, (v, tActive, tRest)) = q * v * tActive + extra
  where
    (q, r) = t `quotRem` (tActive + tRest)
    extra =
      if r <= tActive
        then r * v
        else tActive * v

award :: Int -> [Reindeer] -> [String]
award t rs = concatMap (\(w, dist) -> [w | dist == mx]) ts
  where
    mx = maximum $ fmap snd ts
    ts = fmap (\r@(w, _) -> (w, travelDist t r)) rs

instance Solution Day14 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (extras, rawInput) <- consumeExtra getInputS
    let rs = fmap (consumeOrDie reindeerP) . lines $ rawInput
        tMax = case extras of
          Nothing -> 2503
          Just ~[raw] -> read raw
    answerShow $ maximum $ fmap (travelDist tMax) rs
    answerShow $
      maximum $
        M.elems $ M.fromListWith (+) do
          t <- [1 .. tMax + 1]
          r' <- award t rs
          pure (r', 1 :: Int)
