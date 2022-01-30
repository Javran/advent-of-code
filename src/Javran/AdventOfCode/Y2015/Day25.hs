{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Javran.AdventOfCode.Y2015.Day25
  (
  )
where

import Data.Mod
import Javran.AdventOfCode.GridSystem.RowThenCol.Uldr
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day25 deriving (Generic)

coordToOrd :: Coord -> Int
coordToOrd (r, c) = t1 + c - 1
  where
    -- shift coord to first col
    t0 = r + c - 1
    -- the value at first col
    t1 = ((t0 - 1) * t0 `quot` 2) + 1

coordP :: ReadP Coord
coordP = do
  strP "To continue, please consult the code grid in the manual.  Enter the code at row "
  r <- decimal1P
  strP ", column "
  c <- decimal1P
  strP "."
  pure (r, c)

instance Solution Day25 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    coord <- consumeOrDie (coordP <* char '\n') <$> getInputS
    let z, a :: Mod 33554393
        z = 20151125
        a = 252533
        code i = z * a ^% (i -1)
    answerShow (unMod $ code $ coordToOrd coord)
