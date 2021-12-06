{-# LANGUAGE TemplateHaskell #-}

module Javran.AdventOfCode.Y2021.Main
  ( subMain
  , allSolutions
  )
where

import Data.List
import Javran.AdventOfCode.Infra
import Javran.AdventOfCode.MainMaker
import Javran.AdventOfCode.TH

{- ORMOLU_DISABLE -}
import Javran.AdventOfCode.Y2021.Day1 ()
import Javran.AdventOfCode.Y2021.Day2 ()
import Javran.AdventOfCode.Y2021.Day3 ()
import Javran.AdventOfCode.Y2021.Day4 ()
import Javran.AdventOfCode.Y2021.Day5 ()
import Javran.AdventOfCode.Y2021.Day6 ()
{- ORMOLU_ENABLE -}

allSolutions :: [SomeSolution]
allSolutions =
  sortOn
    (\(SomeSolution s) -> solutionIndex s)
    $collectSolutions

subMain :: SubCmdContext -> IO ()
subMain = mkYearlyMain 2021 allSolutions
