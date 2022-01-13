{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Javran.AdventOfCode.Y2017.Day14
  (
  )
where

import Control.Monad
import Control.Monad.ST
import qualified Data.Array as Arr
import Data.Bits
import Data.Function
import qualified Data.Map.Strict as M
import Data.Word
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.UnionFind.ST (countClusters)
import qualified Javran.AdventOfCode.UnionFind.ST as UF
import Javran.AdventOfCode.Y2017.Day10 (knotHash)

data Day14 deriving (Generic)

type Coord = (Int, Int)

solve :: (Coord -> Bool) -> ST s Int
solve elemAt = do
  let coords = do
        r <- [0 .. 127]
        c <- [0 .. 127]
        let coord = (r, c)
        coord <$ guard (elemAt coord)
  m <-
    fix
      (\go curMap todos -> case todos of
         [] -> pure curMap
         coord@(r, c) : todos' -> do
           pt <- UF.fresh coord
           let connectWith c1 = do
                 rep1 <- UF.repr (curMap M.! c1)
                 UF.union pt rep1
           when (r -1 >= 0 && elemAt (r -1, c)) do
             connectWith (r -1, c)
           when (c -1 >= 0 && elemAt (r, c -1)) do
             connectWith (r, c -1)
           go (M.insert coord pt curMap) todos')
      M.empty
      coords
  countClusters $ M.elems m

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
    answerShow $ runST $ solve elemAt
