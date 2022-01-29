{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2015.Day16
  (
  )
where

import Control.Monad
import Data.Char
import qualified Data.Map.Strict as M
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day16 deriving (Generic)

type Known = (Int, M.Map String Int)

knownP :: ReadP Known
knownP = do
  n <- between (strP "Sue ") (strP ": ") decimal1P
  let pairP = do
        k <- munch1 isLetter
        strP ": "
        v <- decimal1P
        pure (k, v)
  ps <- pairP `sepBy` strP ", "
  pure (n, M.fromList ps)

couldMatch :: Known -> Maybe Int
couldMatch (which, m) = do
  let eKey ~> eVal = case m M.!? eKey of
        Nothing -> pure ()
        Just v -> guard (eVal == v)
  "children" ~> 3
  "cats" ~> 7
  "samoyeds" ~> 2
  "pomeranians" ~> 3
  "akitas" ~> 0
  "vizslas" ~> 0
  "goldfish" ~> 5
  "trees" ~> 3
  "cars" ~> 2
  "perfumes" ~> 1
  pure which

couldMatch2 :: Known -> Maybe Int
couldMatch2 (which, m) = do
  let eKey ~> eVal = case m M.!? eKey of
        Nothing -> pure ()
        Just v ->
          if
              | eKey `elem` ["cats", "trees"] ->
                guard (v > eVal)
              | eKey `elem` ["pomeranians", "goldfish"] ->
                guard (v < eVal)
              | otherwise -> guard (v == eVal)
  "children" ~> 3
  "cats" ~> 7
  "samoyeds" ~> 2
  "pomeranians" ~> 3
  "akitas" ~> 0
  "vizslas" ~> 0
  "goldfish" ~> 5
  "trees" ~> 3
  "cars" ~> 2
  "perfumes" ~> 1
  pure which

instance Solution Day16 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    knowns <- fmap (consumeOrDie knownP) . lines <$> getInputS
    do
      let [ans1] = mapMaybe couldMatch knowns
      answerShow ans1
    do
      let [ans2] = mapMaybe couldMatch2 knowns
      answerShow ans2
