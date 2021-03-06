{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2020.Day1
  (
  )
where

import qualified Data.IntSet as IS
import Javran.AdventOfCode.Prelude

data Day1 deriving (Generic)

instance Solution Day1 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- IS.fromList . fmap (read @Int) . words <$> getInputS
    answerShow (head (solutions xs))
    answerShow (head (solutions2 xs))

solutions :: IS.IntSet -> [] Int
solutions xs = do
  x <- IS.toAscList xs
  let (_, ys) = IS.split x xs
  let y = 2020 - x
  True <- [IS.member y ys]
  pure $ x * y

solutions2 :: IS.IntSet -> [] Int
solutions2 xs = do
  x <- IS.toAscList xs
  let (_, ys) = IS.split x xs
  y <- IS.toAscList ys
  let (_, zs) = IS.split y ys
      z = 2020 - x - y
  True <- [IS.member z zs]
  pure $ x * y * z
