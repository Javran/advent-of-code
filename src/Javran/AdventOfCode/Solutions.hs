{-# LANGUAGE TemplateHaskell #-}

module Javran.AdventOfCode.Solutions
  ( allSolutions
  , allSolutionsSorted
  , getSolution
  )
where

import qualified Data.IntMap.Strict as IM
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
import Javran.AdventOfCode.Y2021.Day7 ()
{- ORMOLU_ENABLE -}

allSolutions :: IM.IntMap (IM.IntMap SomeSolution)
allSolutions = IM.fromListWith (IM.unionWith mergeErr) $ do
  someSol@(SomeSolution s) <- $collectSolutions
  let (yyyy, dd) = solutionIndex s
  pure (yyyy, IM.singleton dd someSol)
  where
    mergeErr (SomeSolution s) _ =
      error $ "entry not identical: (year,day) = " <> show ind
      where
        ind = solutionIndex s

allSolutionsSorted :: [SomeSolution]
allSolutionsSorted = do
  (_, yearSols) <- IM.toAscList allSolutions
  snd <$> IM.toAscList yearSols

getSolution :: Int -> Int -> Maybe SomeSolution
getSolution year day = allSolutions IM.!? year >>= (IM.!? day)
