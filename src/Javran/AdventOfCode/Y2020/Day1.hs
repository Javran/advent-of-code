{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2020.Day1
  ( main
  )
where

import qualified Data.IntSet as IS
import Javran.AdventOfCode.Prelude

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

main :: IO ()
main = do
  xs <- IS.fromList . fmap (read @Int) . words <$> getInput 2020 1
  print (head (solutions xs))
  print (head (solutions2 xs))
