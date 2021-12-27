module Javran.AdventOfCode.NumberTheory
  ( chineseRemainder
  , module Math.NumberTheory.Moduli.Class
  , module Math.NumberTheory.Moduli.Chinese
  )
where

import Control.Monad
import Math.NumberTheory.Moduli.Chinese
import Math.NumberTheory.Moduli.Class

-- Ref: https://github.com/Bodigrim/arithmoi/issues/69
chineseRemainder :: [SomeMod] -> Maybe SomeMod
chineseRemainder (x : xs) = foldM chineseSomeMod x xs
chineseRemainder _ = Just (0 `modulo` 1)
