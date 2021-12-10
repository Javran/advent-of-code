{-# LANGUAGE OverloadedStrings #-}

module Javran.AdventOfCode.Prelude
  ( -- helper functions
    countLength
  , splitOn
  , inRange
  , decodeBinary
  , errInvalid
  , unreachable
  , universe
  , -- infrastructures
    module Javran.AdventOfCode.Infra
  , module Petbox
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

errInvalid :: a
errInvalid = error "invalid input"

unreachable :: a
unreachable = error "unreachable"
