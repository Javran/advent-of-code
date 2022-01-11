{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Javran.AdventOfCode.Y2017.Day4
  (
  )
where

import Data.List
import qualified Data.Map.Strict as M
import Data.Monoid
import qualified Data.Set as S
import Javran.AdventOfCode.Prelude

data Day4 deriving (Generic)

isValid :: [String] -> Bool
isValid xs = l == S.size ys
  where
    (Sum l, ys) = foldMap (\s -> (1, S.singleton s)) xs

isValid2 :: [String] -> Bool
isValid2 xs = all (<= 1) m
  where
    m = M.fromListWith (+) do
      x <- xs
      pure (sort x, 1 :: Int)

instance Solution Day4 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap words . lines <$> getInputS
    answerShow $ countLength isValid xs
    answerShow $ countLength isValid2 xs
