{-# LANGUAGE OverloadedStrings #-}

module Javran.AdventOfCode.Prelude
  ( -- helper functions
    decimal1P
  , consumeAllWithReadP
  , countLength
  , consumeExtraLeadingLines
  , -- infrastructures
    module Javran.AdventOfCode.Infra
  )
where

import Data.Char
import Data.Monoid
import Javran.AdventOfCode.Infra
  ( Solution (..)
  , SolutionContext (..)
  )
import Text.ParserCombinators.ReadP

decimal1P :: (Read i, Integral i) => ReadP i
decimal1P = read <$> munch1 isDigit

consumeAllWithReadP :: ReadP a -> String -> Maybe a
consumeAllWithReadP p xs = case readP_to_S (p <* eof) xs of
  [(v, "")] -> pure v
  _ -> Nothing

countLength :: Foldable f => (a -> Bool) -> f a -> Int
countLength p = getSum . foldMap (\x -> if p x then 1 else 0)


{-
  Examples could contain smaller examples with smaller extra parameters than
  the actual input - to allow solutions to deal with this situation,
  a special section can be introduced to the input data,
  which must be the first section of the input file:

  > # EXAMPLE_EXTRA_BEGIN
  > ... some extra lines ...
  > ... some more extra lines ...
  > # EXAMPLE_EXTRA_END

  and `consumeExtraLeadingLines` cuts this extra section
  as a separate bit of input for a solution to consume.
 -}
consumeExtraLeadingLines :: String -> (Maybe [String], String)
consumeExtraLeadingLines raw = case lines raw of
  "# EXAMPLE_EXTRA_BEGIN" : xs
    | (ys, _ : zs) <-
        span (/= "# EXAMPLE_EXTRA_END") xs ->
      (Just ys, unlines zs)
  _ -> (Nothing, raw)
