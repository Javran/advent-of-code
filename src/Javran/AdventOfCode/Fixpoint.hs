{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Javran.AdventOfCode.Fixpoint
  ( findFix
  )
where

import qualified Data.Containers as C
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M

findFix
  :: forall m k.
  (C.IsMap m, C.MapValue m ~ Int, C.ContainerKey m ~ k)
  => m
  -> [(Int, k)]
  -> ((Int, Int), (k, m))
findFix seen ~((j, x) : xs) = case C.lookup x seen of
  Just i ->
    {- return value indicates that current value `x`, tagged `j`,
       has been seen before with another tag `i`.
     -}
    ((i, j), (x, seen))
  Nothing -> findFix (C.insertMap x j seen) xs
{-# SPECIALIZE findFix :: Ord k => M.Map k Int -> [(Int, k)] -> ((Int, Int), (k, M.Map k Int)) #-}
{-# SPECIALIZE findFix :: IM.IntMap Int -> [(Int, Int)] -> ((Int, Int), (Int, IM.IntMap Int)) #-}
{-# SPECIALIZE findFix :: (Hashable k, Eq k) => HM.HashMap k Int -> [(Int, k)] -> ((Int, Int), (k, HM.HashMap k Int)) #-}
