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
  , universe
  , MinMax (..)
  , minMax
  , minMaxFromPair
  , MinMax2D (..)
  , minMax2D
  , nextCharP
  , -- infrastructures
    module Javran.AdventOfCode.Infra
  , module Petbox
  , module Data.Tuple
  , module Data.Maybe
  , module Data.Bool
  , module Data.Either
  , module GHC.Generics
  , module Data.Function
  , module Data.Ord
  , module Data.Bifunctor
  )
where

import Data.Bifunctor
import Data.Bool
import Data.Char
import Data.Either
import Data.Function
import Data.Ix (inRange)
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Semigroup
import Data.Tuple
import GHC.Generics (Generic)
import Javran.AdventOfCode.Infra
  ( Solution (..)
  , SolutionContext (..)
  , consumeAllWithReadP
  , consumeExtraLeadingLines
  , consumeOrDie
  , decimal1P
  , unreachable
  , todo
  )
import Petbox
import qualified Text.ParserCombinators.ReadP

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

newtype MinMax a = MinMax {getMinMax :: (a, a)}
  deriving (Semigroup) via (Min a, Max a)
  deriving stock (Show, Eq, Ord)

minMax :: a -> MinMax a
minMax a = MinMax (a, a)

minMaxFromPair :: Ord a => (a, a) -> MinMax a
minMaxFromPair (a, b) = MinMax $ if a <= b then (a, b) else (b, a)

newtype MinMax2D u v = MinMax2D {getMinMax2D :: ((u, u), (v, v))}
  deriving (Semigroup) via ((Min u, Max u), (Min v, Max v))
  deriving stock (Show)

minMax2D :: (u, v) -> MinMax2D u v
minMax2D (u, v) = MinMax2D ((u, u), (v, v))

nextCharP :: Text.ParserCombinators.ReadP.ReadP Char
nextCharP = Text.ParserCombinators.ReadP.get
