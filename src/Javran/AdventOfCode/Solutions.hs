{-# LANGUAGE TemplateHaskell #-}

module Javran.AdventOfCode.Solutions
  ( getSolution
  )
where

import Data.List
import qualified Data.Map.Strict as M
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
import Javran.AdventOfCode.Y2020.Day14 ()
import Javran.AdventOfCode.Y2020.Day15 ()
import Javran.AdventOfCode.Y2020.Day16 ()
import Javran.AdventOfCode.Y2020.Day17 ()
import Javran.AdventOfCode.Y2020.Day18 ()
import Javran.AdventOfCode.Y2020.Day19 ()
import Javran.AdventOfCode.Y2020.Day20 ()
import Javran.AdventOfCode.Y2020.Day21 ()
import Javran.AdventOfCode.Y2020.Day22 ()
import Javran.AdventOfCode.Y2020.Day23 ()
import Javran.AdventOfCode.Y2020.Day24 ()
import Javran.AdventOfCode.Y2020.Day25 ()
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

getSolution :: Int -> Int -> Maybe SomeSolution
getSolution year day = allSolutions' M.!? (year, day)
  where
    allSolutions' = M.fromList $ do
      someSol@(SomeSolution s) <- allSolutions
      let sInd = solutionIndex s
      pure (sInd, someSol)
