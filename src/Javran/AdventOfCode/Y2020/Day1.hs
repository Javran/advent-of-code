{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2020.Day1
  ( main
  )
where

import qualified Data.IntSet as IS

getInput :: IO IS.IntSet
getInput = do
  xs <- fmap (read @Int) . words <$> readFile "data/2020/day/1/input"
  pure $ IS.fromList xs

solutions :: IS.IntSet -> [] (Int, Int)
solutions xs = do
  x <- IS.toAscList xs
  let (_, ys) = IS.split x xs
  True <- [IS.member (2020 - x) ys]
  pure (x, 2020 - x)

main :: IO ()
main = getInput >>= print . uncurry (*) . head . solutions
