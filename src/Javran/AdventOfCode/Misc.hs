module Javran.AdventOfCode.Misc
  ( rotateLeftBy
  , rotateRightBy
  , internalize
  , commitLeft1
  , nthPermutation
  )
where

import qualified Data.Array as Arr
import Data.Containers.ListUtils
import Data.List
import qualified Data.Map.Strict as M
import Data.Tuple
import Text.ParserCombinators.ReadP (ReadP, (<++))

{-
  This module contains things that could probably be in Prelude,
  but not as stable / well-tested / frequently used.
 -}

{-
  Rotates a known-length list to left or right.
  n > 0 (in other words, non-empty list), 0 <= offset <= n.

  It is intentional that these two functions lean towards
  performance rather than being convenient -
  how to compute list length and whether to apply modulo on offsets
  is not our concern here.
 -}
rotateLeftBy, rotateRightBy :: Int -> Int -> [a] -> [a]
rotateLeftBy _n offset xs = zs <> ys
  where
    (ys, zs) = splitAt offset xs
rotateRightBy n offset xs = zs <> ys
  where
    (ys, zs) = splitAt (n - offset) xs

{-
  Builds one-to-one mappings between a known set of values (typically Strings)
  and Int.
 -}
internalize :: Ord a => [a] -> (a -> Int, Int -> a)
internalize xs = ((m M.!), (arr Arr.!))
  where
    paired = zip (nubOrd xs) [0 ..]
    m = M.fromList paired
    arr = Arr.array (0, M.size m - 1) (fmap swap paired)

commitLeft1 :: Foldable t => t (ReadP a) -> ReadP a
commitLeft1 = foldr1 (<++)


{-
  TODO: use nthPermutation to generate input string
  for QuickCheck-ing Y2016.Day21.
 -}

{-
  Finds the n-th permutation of a list:

  nthPermutation n which xs

  - length xs == n
  - 0 <= which < product [1..n]

 -}
nthPermutation :: Int -> Int -> [a] -> [a]
nthPermutation n which =
  if n <= 1
    then id
    else selects decisions
  where
    decisions = go mods which
      where
        mods = reverse (scanl' (*) 1 [2 .. n -1])
        go xs v = case xs of
          [] -> [v]
          x : xs' ->
            let (q, r) = v `quotRem` x
             in q : go xs' r
    select i xs = (v, ys <> zs)
      where
        (ys, v : zs) = splitAt i xs
    selects indices xs = case indices of
      [] -> []
      i : indices' ->
        let (v, xs') = select i xs
         in v : selects indices' xs'
