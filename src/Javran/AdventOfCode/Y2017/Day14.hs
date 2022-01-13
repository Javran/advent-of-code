{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
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
           let mayConnectWith c1 = case curMap M.!? c1 of
                 Nothing -> pure ()
                 Just pt' -> UF.union pt pt'
           mayConnectWith (r -1, c)
           mayConnectWith (r, c -1)
           go (M.insert coord pt curMap) todos')
      M.empty
      coords
  UF.countClusters $ M.elems m

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
        elemAt :: Coord -> Bool
        elemAt (r, c) = testBit v (7 - cRem)
          where
            (cQuot, cRem) = quotRem c 8
            v = disk Arr.! (r, cQuot)
    answerShow $ sum $ fmap popCount $ Arr.elems disk
    answerShow $ runST $ solve elemAt
