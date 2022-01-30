{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
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

couldMatch :: (String -> Int -> Int -> Bool) -> Known -> Maybe Int
couldMatch matcher (which, m) =
  which <$ do
    let eKey ~> eVal =
          guard $ maybe True (matcher eKey eVal) (m M.!? eKey)
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

instance Solution Day16 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    knowns <- fmap (consumeOrDie knownP) . lines <$> getInputS
    do
      let matcher _eKey eVal v = v == eVal
          [ans1] = mapMaybe (couldMatch matcher) knowns
      answerShow ans1
    do
      let matcher eKey eVal v
            | eKey `elem` ["cats", "trees"] = v > eVal
            | eKey `elem` ["pomeranians", "goldfish"] = v < eVal
            | otherwise = v == eVal
          [ans2] = mapMaybe (couldMatch matcher) knowns
      answerShow ans2
