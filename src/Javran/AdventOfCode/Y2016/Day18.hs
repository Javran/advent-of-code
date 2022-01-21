{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2016.Day18
  (
  )
where

import Data.List.Split hiding (sepBy)
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.TestExtra
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day18 deriving (Generic)

data Tile = Safe | Trap deriving (Eq, Ord)

tileP :: ReadP Tile
tileP = (Safe <$ char '.') <++ (Trap <$ char '^')

rule :: [Tile] -> Tile
rule local =
  if local
    `elem` [ [Trap, Trap, Safe]
           , [Safe, Trap, Trap]
           , [Trap, Safe, Safe]
           , [Safe, Safe, Trap]
           ]
    then Trap
    else Safe

nextRow :: [Tile] -> [Tile]
nextRow xs = fmap rule $ divvy 3 1 (Safe : xs <> [Safe])

_pprRow :: [Tile] -> String
_pprRow = fmap \case
  Safe -> '.'
  Trap -> '^'

instance Solution Day18 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (extraOpts, rawInput) <- consumeExtra getInputS
    let initRow = consumeOrDie (many1 tileP) . head . lines $ rawInput
        allRows = iterate nextRow initRow
    do
      let takeN = take case extraOpts of
            Nothing -> 40
            Just ~[raw] -> read raw
          room = takeN allRows
      answerShow $ countLength (== Safe) $ concat room
    do
      let takeN = take case extraOpts of
            Nothing -> 400000
            Just _ -> 4000
      answerShow $ countLength (== Safe) $ concat (takeN allRows)
