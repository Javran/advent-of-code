{-# LANGUAGE NoMonomorphismRestriction #-}

module Javran.AdventOfCode.NumberTheory
  ( chineseRemainder
  , sumOfDivisors
  , module Math.NumberTheory.Moduli.Class
  , module Math.NumberTheory.Moduli.Chinese
  )
where

import Control.Monad
import Math.NumberTheory.Moduli.Chinese
import Math.NumberTheory.Moduli.Class
import Math.NumberTheory.Primes

-- Ref: https://github.com/Bodigrim/arithmoi/issues/69
chineseRemainder :: [SomeMod] -> Maybe SomeMod
chineseRemainder (x : xs) = foldM chineseSomeMod x xs
chineseRemainder _ = Just (0 `modulo` 1)

-- https://math.stackexchange.com/a/22723/139439
sumOfDivisors :: UniqueFactorisation c => c -> c
sumOfDivisors = product . fmap f . factorise
  where
    f (p, m) = sum (take (1 + fromIntegral m) $ iterate (* unPrime p) 1)
{-# INLINABLE sumOfDivisors #-}
{-# SPECIALIZE sumOfDivisors :: Int -> Int #-}
