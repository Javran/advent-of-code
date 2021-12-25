{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Javran.AdventOfCode.Y2020.Day6
  (
  )
where

import qualified Data.Set as S
import Javran.AdventOfCode.Prelude

data Day6 deriving (Generic)

instance Solution Day6 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    rawLines <- lines <$> getInputS
    let groups = fmap S.fromList <$> splitOn [""] rawLines
        unions = fmap (foldr1 S.union) groups
        intersects = fmap (foldr1 S.intersection) groups
    answerShow $ sum (fmap S.size unions)
    answerShow $ sum (fmap S.size intersects)
