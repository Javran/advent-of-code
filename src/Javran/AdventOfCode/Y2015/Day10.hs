{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2015.Day10
  (
  )
where

import qualified Data.List.NonEmpty as NE
import Javran.AdventOfCode.Prelude

data Day10 deriving (Generic)

lookAndSay :: [Int] -> [Int]
lookAndSay xs = xs'
  where
    look v = (NE.length v, NE.head v)
    say (n, a) = intToDigits n <> [a]
    xs' = concatMap (say . look) $ NE.group xs

instance Solution Day10 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap chToInt . head . lines <$> getInputS
    let progression = iterate lookAndSay xs
    answerShow $ length $ progression !! 40
    answerShow $ length $ progression !! 50
