{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Javran.AdventOfCode.Prelude
  ( -- helper functions
    countLength
  , splitOn
  , inRange
  , decodeBinary
  , chToInt
  , udlrOfCoord
  , errInvalid
  , unreachable
  , todo
  , universe
  , MinMax (..)
  , minMax
  , MinMax2D (..)
  , minMax2D
  , -- infrastructures
    module Javran.AdventOfCode.Infra
  , module Petbox
  )
where

import Data.Bifunctor
import Data.Bool
import Data.Char
import Data.Ix (inRange)
import Data.List
import Data.List.Split
import Data.Monoid
import Data.Semigroup
import Javran.AdventOfCode.Infra
  ( Solution (..)
  , SolutionContext (..)
  , consumeAllWithReadP
  , consumeExtraLeadingLines
  , consumeOrDie
  , decimal1P
  )
import Petbox

universe :: (Enum a, Bounded a) => [a]
universe = [minBound .. maxBound]

countLength :: Foldable f => (a -> Bool) -> f a -> Int
countLength p = getSum . foldMap (\x -> if p x then 1 else 0)

decodeBinary :: (Foldable t, Num a) => t Bool -> a
decodeBinary = foldl (\acc i -> acc * 2 + bool 0 1 i) 0

chToInt :: Char -> Int
chToInt ch = ord ch - ord '0'

udlrOfCoord :: (Int, Int) -> [(Int, Int)]
udlrOfCoord c =
  fmap
    ($ c)
    [ first pred
    , first succ
    , second pred
    , second succ
    ]

errInvalid :: a
errInvalid = error "invalid input"

unreachable :: a
unreachable = error "unreachable"

todo :: a
todo = error "todo"

newtype MinMax a = MinMax {getMinMax :: (a, a)}
  deriving (Semigroup) via (Min a, Max a)
  deriving stock Show

minMax :: a -> MinMax a
minMax a = MinMax (a, a)

newtype MinMax2D u v = MinMax2D {getMinMax2D :: ((u, u), (v, v))}
  deriving (Semigroup) via ((Min u, Max u), (Min v, Max v))
  deriving stock Show

minMax2D :: (u, v) -> MinMax2D u v
minMax2D (u, v) = MinMax2D ((u, u), (v, v))
