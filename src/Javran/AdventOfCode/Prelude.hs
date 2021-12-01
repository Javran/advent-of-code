{-# LANGUAGE OverloadedStrings #-}

module Javran.AdventOfCode.Prelude
  ( -- helper functions
    decimal1P
  , consumeAllWithReadP
  , countLength
  , -- infrastructures
    module Javran.AdventOfCode.Infra
  )
where

import Data.Char
import Javran.AdventOfCode.Infra
  ( Solution (..)
  , SolutionContext (..)
  )
import Text.ParserCombinators.ReadP
import Data.Monoid

decimal1P :: (Read i, Integral i) => ReadP i
decimal1P = read <$> munch1 isDigit

consumeAllWithReadP :: ReadP a -> String -> Maybe a
consumeAllWithReadP p xs = case readP_to_S (p <* eof) xs of
  [(v, "")] -> pure v
  _ -> Nothing

countLength :: Foldable f => (a -> Bool) -> f a -> Int
countLength p = getSum . foldMap (\x -> if p x then 1 else 0)
