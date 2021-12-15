{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Javran.AdventOfCode.Y2021.Day14
  (
  )
where

import Data.Char
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, many)

data Day14 deriving (Generic)

ruleP :: ReadP (String, Char)
ruleP = do
  xs <- munch1 isAlpha
  _ <- string " -> "
  y <- get
  pure (xs, y)

type Rules = M.Map Seg2 Char

type Seg2 = [] Char -- exactly two Chars.

type CompactRep = M.Map Seg2 Int

type RecoverFreq = CompactRep -> M.Map Char Int

toCompact :: String -> (RecoverFreq, CompactRep)
toCompact xs = (recoverFreq,) $ M.fromListWith (+) do
  a <- divvy 2 1 xs
  pure (a, 1)
  where
    recoverFreq :: RecoverFreq
    recoverFreq =
      M.map (`quot` 2)
        . M.adjust succ (head xs)
        . M.adjust succ (last xs)
        . compactCount

step :: Rules -> CompactRep -> CompactRep
step rules st = M.fromListWith (+) do
  ([x, y], freq) <- M.toList st
  let a = rules M.! [x, y]
  [([x, a], freq), ([a, y], freq)]

compactCount :: M.Map Seg2 Int -> M.Map Char Int
compactCount m = M.fromListWith (+) do
  ([a, b], freq) <- M.toList m
  [(a, freq), (b, freq)]

minMaxDiff :: [Int] -> Int
minMaxDiff xs = b - a
  where
    Just (MinMax (a,b)) = foldMap (Just . minMax) xs

instance Solution Day14 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    [[initSt], rawRules] <- splitOn [""] . lines <$> getInputS
    let rules = M.fromList (fmap (consumeOrDie ruleP) rawRules)
        (recoverFreq, initSt') = toCompact initSt
        progression = iterate (step rules) initSt'
        solveAt n = minMaxDiff $ M.elems $ recoverFreq (progression !! n)
    answerShow $ solveAt 10
    answerShow $ solveAt 40
