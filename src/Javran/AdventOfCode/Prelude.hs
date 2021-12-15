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
