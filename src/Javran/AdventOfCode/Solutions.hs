module Javran.AdventOfCode.Solutions
  ( getSolution
  )
where

import qualified Data.Map.Strict as M
import Javran.AdventOfCode.Infra
import qualified Javran.AdventOfCode.Y2020.Main as Y2020
import qualified Javran.AdventOfCode.Y2021.Main as Y2021

getSolution :: Int -> Int -> Maybe SomeSolution
getSolution year day = allSolutions M.!? (year, day)
  where
    allSolutions = M.fromList $ do
      someSol@(SomeSolution s) <- Y2020.allSolutions <> Y2021.allSolutions
      let sInd = solutionIndex s
      pure (sInd, someSol)
