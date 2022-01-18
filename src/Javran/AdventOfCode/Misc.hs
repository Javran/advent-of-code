module Javran.AdventOfCode.Misc
  ( rotateLeftBy
  , rotateRightBy
  )
where

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
