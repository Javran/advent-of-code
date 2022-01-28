{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2015.Day3
  (
  )
where

import Control.Applicative
import Data.List
import qualified Data.Set as S
import Javran.AdventOfCode.GridSystem.RowThenCol.Nwse
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day3 deriving (Generic)

dirP :: ReadP Dir
dirP =
  (N <$ char '^')
    <++ (S <$ char 'v')
    <++ (W <$ char '<')
    <++ (E <$ char '>')

instance Solution Day3 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    dirs <- consumeOrDie (many dirP) . head . lines <$> getInputS
    let o = (0, 0)
    do
      let locs = scanl' (flip applyDir) o dirs
      answerShow $ S.size $ S.fromList locs
    do
      let locs =
            scanl'
              (\locPair (sel, dir) -> sel (applyDir dir) locPair)
              (o, o)
              (zip (cycle [first, second]) dirs)
      answerShow $
        S.size $ S.fromList do
          (a, b) <- locs
          [a, b]
