{-# LANGUAGE OverloadedStrings #-}

module Javran.AdventOfCode.Prelude
  ( -- helper functions
    countLength
  , splitOn
  , inRange
  , decodeBinary
  , errInvalid
  , unreachable
  , pick
  , pickInOrder
  , universe
  , -- infrastructures
    module Javran.AdventOfCode.Infra
  )
where

import Data.Bool
import Data.Ix (inRange)
import Data.List
import Data.List.Split
import Data.Monoid
import Javran.AdventOfCode.Infra
  ( Solution (..)
  , SolutionContext (..)
  , consumeAllWithReadP
  , consumeExtraLeadingLines
  , decimal1P
  )

universe :: (Enum a, Bounded a) => [a]
universe = [minBound .. maxBound]

countLength :: Foldable f => (a -> Bool) -> f a -> Int
countLength p = getSum . foldMap (\x -> if p x then 1 else 0)

decodeBinary :: (Foldable t, Num a) => t Bool -> a
decodeBinary = foldl (\acc i -> acc * 2 + bool 0 1 i) 0

errInvalid :: a
errInvalid = error "invalid input"

unreachable :: a
unreachable = error "unreachable"

-- | non-deterministically picking an element from the given list,
--   separating the selected element and all other remaining elements
--   the list order is preserved
--   e.g. pick [1,2,3] == [(1,[2,3]),(2,[1,3]),(3,[1,2])]
pick :: [a] -> [(a, [a])]
pick xs = map splitAux (init $ zip (inits xs) (tails xs))
  where
    splitAux (ls, v : rs) = (v, ls ++ rs)
    splitAux _ = error "cannot split empty list"

{-
  like "pick", but whenever an element picked,
  all elements before it will be dropped. This has the effect of only picking
  elements in order.
 -}
pickInOrder :: [a] -> [] (a, [a])
pickInOrder [] = []
pickInOrder (x : xs) = (x, xs) : pickInOrder xs
