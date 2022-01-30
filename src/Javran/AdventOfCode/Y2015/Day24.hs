{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2015.Day24
  (
  )
where

import Data.List
import Javran.AdventOfCode.Prelude

data Day24 deriving (Generic)

{-
  Picks from a sorted list to form a subsequence that sums up to a specific number.
 -}
collectExact :: Int -> [Int] -> [] ([Int], [Int])
collectExact tgt inp = do
  ys <- aux tgt inp
  pure (ys, inp \\ ys)
  where
    aux target xs =
      if target == 0
        then pure []
        else do
          (y, ys) <- pickInOrder $ takeWhile (<= target) xs
          let target' = target - y
          (y :) <$> aux target' ys

solve :: [Int] -> Int -> Int -> Int
solve xs n total = snd $ head $ filter isSolvable sortedFirstPart
  where
    (target, 0) = total `quotRem` n
    -- no matter how it's partitioned, the partition should always be in this set.
    onePart = fmap (\(p, _) -> (p, product p)) $ collectExact target xs
    sortedFirstPart =
      -- order by len then QE.
      sortOn (first length) onePart
    isSolvable (p1, _) = not . null $ do
      {-
        The only thing matters is the first part,
        it is a solution as long as remaining part is solvable.
       -}
      fix
        (\go zs cnt ->
           if cnt > 0
             then do
               (_, zs') <- collectExact target zs
               go zs' (cnt -1)
             else pure ())
        (xs \\ p1)
        (n - 2) -- minus 2 to account for the last part and the fixed first part

instance Solution Day24 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- sort . fmap read . lines <$> getInputS
    let total = sum xs
    answerShow $ solve xs 3 total
    answerShow $ solve xs 4 total

