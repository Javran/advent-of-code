{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2017.Day10
  ( knotHash
  )
where

import Control.Monad
import Data.Bits
import Data.Char
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word
import Javran.AdventOfCode.Misc (rotateLeftBy, rotateRightBy)
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.TestExtra
import Text.Printf

data Day10 deriving (Generic)

{-
  A Circle (xs, offset) represents a list for the hash process,
  where:
  - the head of `xs` is always the current position
  - the list is rotated to left by `offset` positions,
    so that we can recover the underlying list by rotating to right the same amount.
 -}
type Circle = ([Word8], Int)

step :: Int -> Int -> Int -> Circle -> Circle
step n len skipSize (xs0, offset) = (xs2, (offset + offset') `rem` n)
  where
    xs1 = let (ys, zs) = splitAt len xs0 in reverse ys <> zs
    offset' = len + skipSize
    xs2 = rotateLeftBy n (len + skipSize) xs1

viaCircle :: Int -> (Circle -> Circle) -> [Word8] -> [Word8]
viaCircle n f xs = rotateRightBy n offset ys
  where
    (ys, offset) = f (xs, 0)

knotHashInternal :: Int -> [Int] -> [Word8]
knotHashInternal n lenSeq =
  viaCircle
    n
    (\c0 ->
       foldl' (\cir (len, skipSize) -> step n len skipSize cir) c0 $
         zip lenSeq [0 ..])
    [0 .. fromIntegral (n -1)]

mkFullLenSeq :: [Char] -> [Int]
mkFullLenSeq xs = concat (replicate 64 ys)
  where
    ys = fmap (fromIntegral . ord) xs <> [17, 31, 73, 47, 23]

knotHashListBased :: String -> [Word8]
knotHashListBased xs = fmap (foldl1' xor) $ chunksOf 16 sparse
  where
    lenSeq = mkFullLenSeq xs
    sparse = knotHashInternal 256 lenSeq

knotHashFast :: String -> [Word8]
knotHashFast xs = dense
  where
    dense = fmap (\i -> VU.foldl1' xor $ VU.unsafeSlice i 16 sparse) [0, 16 .. 255]
    sparse = VU.create do
      vec <- VU.unsafeThaw (VU.fromListN 256 [0 ..])
      let lenSeq :: [Int]
          lenSeq = mkFullLenSeq xs
      fix
        (\go curIter curInd ->
           case curIter of
             [] -> pure vec
             (len :: Int, stepSize :: Int) : nextIter -> do
               let performRev i j = when (i < j) do
                     let i' = i .&. 0xFF
                         j' = j .&. 0xFF
                     VUM.unsafeSwap vec i' j'
                     performRev (i + 1) (j -1)
               performRev curInd (curInd + len -1)
               let nextInd = (curInd + len + stepSize) .&. 0xFF
               go nextIter nextInd)
        (zip lenSeq [0 ..])
        0

knotHash :: String -> [Word8]
knotHash = if useFast then knotHashFast else knotHashListBased
  where
    useFast = True

instance Solution Day10 where
  solutionRun _ SolutionContext {getInputS, answerShow, answerS} = do
    (extraOps, rawContent) <- consumeExtra getInputS
    let [raw] = lines rawContent
        (runPart1, runPart2) = shouldRun extraOps
    when runPart1 do
      let lenSeq = fmap (read @Int) . splitOn "," $ raw
          n = case extraOps of
            Just ~("part1" : rawN : _) -> read rawN
            Nothing -> 256
          x : y : _ = knotHashInternal n lenSeq
      answerShow $ x * y
    when runPart2 do
      let ans = knotHashFast raw
          hexStr :: Word8 -> String
          hexStr v = printf "%02x" v
      answerS (concatMap hexStr ans)
