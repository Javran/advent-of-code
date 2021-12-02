{-# LANGUAGE TemplateHaskell #-}

module Javran.AdventOfCode.Y2020.Main
  ( subMain
  , allSolutions
  )
where

import Data.List
import Javran.AdventOfCode.Infra
import Javran.AdventOfCode.TH

{- ORMOLU_DISABLE -}
import Javran.AdventOfCode.Y2020.Day1 ()
import Javran.AdventOfCode.Y2020.Day2 ()
import Javran.AdventOfCode.Y2020.Day3 ()
import Javran.AdventOfCode.Y2020.Day4 ()
import Javran.AdventOfCode.Y2020.Day5 ()
import Javran.AdventOfCode.Y2020.Day6 ()
import Javran.AdventOfCode.Y2020.Day7 ()
import Javran.AdventOfCode.Y2020.Day8 ()
import Javran.AdventOfCode.Y2020.Day9 ()
import Javran.AdventOfCode.Y2020.Day10 ()
import Javran.AdventOfCode.Y2020.Day11 ()
import Javran.AdventOfCode.Y2020.Day12 ()
import Javran.AdventOfCode.Y2020.Day13 ()
{- ORMOLU_ENABLE -}

allSolutions :: [SomeSolution]
allSolutions =
  sortOn
    (\(SomeSolution s) -> solutionIndex s)
    $collectSolutions

subMain :: String -> IO ()
subMain = mkYearlyMain 2020 allSolutions
