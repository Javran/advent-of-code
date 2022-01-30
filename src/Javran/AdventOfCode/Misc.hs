module Javran.AdventOfCode.Misc
  ( rotateLeftBy
  , rotateRightBy
  , internalize
  , commitLeft1
  )
where

import qualified Data.Array as Arr
import Data.Containers.ListUtils
import qualified Data.Map.Strict as M
import Data.Tuple
import Text.ParserCombinators.ReadP (ReadP, (<++))

{-
  This module contains things that could probably be in Prelude,
  but not as stable / well-tested / frequently used.
 -}

{-
  Rotates a known-length list to left or right.
  n > 0, 0 <= offset <= n.

  TODO: QuickCheck, make this accept nums in other ranges.

 -}
rotateLeftBy, rotateRightBy :: Int -> Int -> [a] -> [a]
rotateLeftBy n offset xs = take n $ drop offset $ cycle xs
rotateRightBy n offset = rotateLeftBy n (n - offset)

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
