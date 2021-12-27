{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2018.Day1
  (
  )
where

import qualified Data.IntSet as IS
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day1 deriving (Generic)

signedIntP :: ReadP Int
signedIntP = do
  applySign <- (id <$ char '+') <++ (negate <$ char '-')
  n <- decimal1P
  pure $ applySign n

solve :: IS.IntSet -> [Int] -> Int
solve seen = \case
  x : xs ->
    if IS.member x seen
      then x
      else solve (IS.insert x seen) xs
  _ -> unreachable

instance Solution Day1 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (consumeOrDie signedIntP) . lines <$> getInputS
    answerShow (sum xs)
    answerShow (solve IS.empty $ scanl1 (+) (cycle xs))
