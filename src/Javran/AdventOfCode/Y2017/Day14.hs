{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2017.Day14
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import qualified Data.Array as Arr
import Data.Bits
import Data.Char
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.UnionFind.ST as UF
import qualified Data.Vector as V
import Data.Word
import Debug.Trace
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2017.Day10 (knotHash)
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day14 deriving (Generic)

type Coord = (Int, Int)

-- https://github.com/nominolo/union-find/issues/12#issuecomment-647090797
cluster :: Ord a => [(UF.Point s a, b)] -> ST s (M.Map a [b])
cluster pairs =
  M.map ($ []) . M.fromListWith (flip (.))
    <$> mapM
      (\(uf, c) -> do
         cRep <- UF.descriptor =<< UF.repr uf
         pure (cRep, (c :)))
      pairs

solve elemAt = do
  (paired :: [(Coord, UF.Point s Coord)]) <-
    concat
      <$> (forM [0 .. 127] \r ->
             catMaybes
               <$> (forM [0 .. 127] \c -> do
                      let coord = (r, c)
                      if (elemAt (r, c))
                        then do
                          pt <- UF.fresh coord
                          pure (Just (coord, pt))
                        else pure Nothing))
  let pMap = M.fromList paired
      connect c0 c1 = do
        rep0 <- UF.repr (pMap M.! c0)
        rep1 <- UF.repr (pMap M.! c1)
        unless (rep0 == rep1) do
          UF.union rep0 rep1

  forM_ [0 .. 127] \r ->
    forM_ [0 .. 127] \c ->
      when
        (elemAt (r, c))
        do
          when
            (r -1 >= 0 && elemAt (r -1, c))
            do
              connect (r, c) (r -1, c)
          when
            (c -1 >= 0 && elemAt (r, c -1))
            do
              connect (r, c) (r, c -1)
  cluster (fmap swap paired)

instance Solution Day14 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    key <- head . lines <$> getInputS
    let disk :: Arr.Array (Int, Int) Word8
        disk = Arr.array ((0, 0), (127, 15)) do
          let keys = fmap (\i -> key <> "-" <> show i) [0 .. 127 :: Int]
              rows = fmap knotHash keys
          (r, rs) <- zip [0 ..] rows
          (c, x) <- zip [0 ..] rs
          pure ((r, c), x)
        elemAt :: (Int, Int) -> Bool
        elemAt (r, c) = testBit v (7 - cRem)
          where
            (cQuot, cRem) = quotRem c 8
            v = disk Arr.! (r, cQuot)
    answerShow $ sum $ fmap popCount $ Arr.elems disk
    answerShow $ M.size $ runST $ solve elemAt
