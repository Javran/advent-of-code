{-# LANGUAGE OverloadedStrings #-}

module Javran.AdventOfCode.Prelude
  ( -- helper functions
    decimal1P
  , consumeAllWithReadP
  , countLength
  , consumeExtraLeadingLines
  , splitOn
  , inRange
  , decodeBinary
  , errInvalid
  , unreachable
  , pick
  , pickInOrder
  , universe
  , extractSection
  , -- infrastructures
    module Javran.AdventOfCode.Infra
  )
where

import Data.Bool
import Data.Char
import Data.Ix (inRange)
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Monoid
import Javran.AdventOfCode.Infra
  ( Solution (..)
  , SolutionContext (..)
  )
import Text.ParserCombinators.ReadP

universe :: (Enum a, Bounded a) => [a]
universe = [minBound .. maxBound]

decimal1P :: (Read i, Integral i) => ReadP i
decimal1P = read <$> munch1 isDigit

consumeAllWithReadP :: ReadP a -> String -> Maybe a
consumeAllWithReadP p xs = case readP_to_S (p <* eof) xs of
  [(v, "")] -> pure v
  _ -> Nothing

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
consumeExtraLeadingLines raw =
  extractSection
    "# EXAMPLE_EXTRA_BEGIN"
    "# EXAMPLE_EXTRA_END"
    (Nothing, raw)
    (\_pre _bm sec _em post -> (Just sec, unlines post))
    (lines raw)

extractSection :: Eq t => t -> t -> a -> ([t] -> t -> [t] -> t -> [t] -> a) -> [t] -> a
extractSection beginMarker endMarker defVal onSuccess xs = fromMaybe defVal $ do
  (ys0, bm : remaining0) <- pure $ span (/= beginMarker) xs
  (ys1, em : remaining1) <- pure $ span (/= endMarker) remaining0
  pure $ onSuccess ys0 bm ys1 em remaining1
