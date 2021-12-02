{-# LANGUAGE TemplateHaskell #-}

module Javran.AdventOfCode.Y2021.Main
  ( subMain
  , allSolutions
  )
where

import Data.List
import Javran.AdventOfCode.Infra
import Javran.AdventOfCode.TH

{- ORMOLU_DISABLE -}
import Javran.AdventOfCode.Y2021.Day1 ()
import Javran.AdventOfCode.Y2021.Day2 ()
{- ORMOLU_ENABLE -}

allSolutions :: [SomeSolution]
allSolutions =
  sortOn
    (\(SomeSolution s) -> solutionIndex s)
    $collectSolutions

subMain :: String -> IO ()
subMain = mkYearlyMain 2021 allSolutions
