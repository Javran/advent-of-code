{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2017.Day10
  (
  )
where

import Control.Monad
import Data.Bits
import Data.Char
import Data.List
import Data.List.Split hiding (sepBy)
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.TestExtra
import Javran.AdventOfCode.Y2017.Day6 (rotateLeftBy, rotateRightBy)
import Text.Printf

data Day10 deriving (Generic)

{-
  A Circle (xs, offset) represents a list for the hash process,
  where:
  - the head of `xs` is always the current position
  - the list is rotated to left by `offset` positions,
    so that we can recover the underlying list by rotating to right the same amount.
 -}
type Circle = ([Int], Int)

step :: Int -> Int -> Int -> Circle -> Circle
step n len skipSize (xs0, offset) = (xs2, (offset + offset') `rem` n)
  where
    xs1 = let (ys, zs) = splitAt len xs0 in reverse ys <> zs
    offset' = len + skipSize
    xs2 = rotateLeftBy n (len + skipSize) xs1

viaCircle :: Int -> (Circle -> Circle) -> [Int] -> [Int]
viaCircle n f xs = rotateRightBy n offset ys
  where
    (ys, offset) = f (xs, 0)

knotHash :: Int -> [Int] -> [Int]
knotHash n lenSeq =
  viaCircle
    n
    (\c0 ->
       foldl' (\cir (len, skipSize) -> step n len skipSize cir) c0 $
         zip lenSeq [0 ..])
    [0 .. n -1]

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
          x : y : _ = knotHash n lenSeq
      answerShow $ x * y
    when runPart2 do
      let lenSeq = fmap ord raw <> [17, 31, 73, 47, 23]
          sparse = knotHash 256 (concat $ replicate 64 lenSeq)
          dense = fmap (foldl1' xor) $ chunksOf 16 sparse
          hexStr :: Int -> String
          hexStr v = printf "%02x" v
      answerS (concatMap hexStr dense)
