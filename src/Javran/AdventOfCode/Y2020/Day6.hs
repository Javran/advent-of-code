{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Javran.AdventOfCode.Y2020.Day6
  (
  )
where

import qualified Data.List.Split as LSplit
import qualified Data.Set as S
import Javran.AdventOfCode.Prelude

data Day6

instance Solution Day6 where
  solutionIndex _ = (2020, 6)
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    rawLines <- lines <$> getInputS
    let groups = fmap S.fromList <$> LSplit.splitOn [""] rawLines
        unions = fmap (foldr1 S.union) groups
        intersects = fmap (foldr1 S.intersection) groups
    answerShow $ sum (fmap S.size unions)
    answerShow $ sum (fmap S.size intersects)
