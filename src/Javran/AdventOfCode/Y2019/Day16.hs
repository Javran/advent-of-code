{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2019.Day16
  (
  )
where

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude

data Day16 deriving (Generic)

genSeq :: Int -> [Int]
genSeq n = tail (cycle basePat)
  where
    basePat = concatMap (replicate n) [0, 1, 0, -1]

lastDigit :: Int -> Int
lastDigit n = case compare n 0 of
  GT -> n `rem` 10
  EQ -> 0
  LT -> - (n `mod` (-10))

onePhase :: Int -> [Int] -> [Int]
onePhase len xs =
  (\i -> lastDigit $ sum $ zipWith (*) xs (genSeq i)) <$> [1 .. len]

{-
  Part 2 Note:

  Observer how the last 4 digits envolve for input signal 12345678:

  5678 -> 6158 -> 0438 -> 5518 -> 9498

  note that each step is just summation from right without carry:

  8 + [0] => 8
  7 + [8] => (1)5
  6 + [5] => (1)1
  5 + [1] => 6

  Indeed, base pattern 0,1,0,-1 expands faster than the length of the sequence,
  so that we end up with a triangle of coefficient:

  ... 1 1 1 1
  ... 0 1 1 1
  ... 0 0 1 1
  ... 0 0 0 1

  which *is* the summation from right without carry.

  This solution relies on the fact that the puzzle only asks
  for a message whose entirety is in the second half of the
  10000-times-repeated message.

  If we cut the message and reverse it (call it mRev) so that the message of interest is
  the last 8 digits of mRev, we can just do this summation 100 times
  on mRev, obtain last 8 digits after we are done. The reverse of it is the answer.
 -}
performSndHalfFftRev :: [Int] -> Int
performSndHalfFftRev xs = runST do
  let l = length xs
  vs2 <- do
    vs <- VU.unsafeThaw (VU.fromListN @Int l xs)
    replicateM_ 100 do
      forM_ [1 .. l -1] $ \i -> do
        x <- VUM.unsafeRead vs (i -1)
        y <- VUM.unsafeRead vs i
        VUM.unsafeWrite vs i ((x + y) `rem` 10)
    let vs1 = VUM.drop (l - 8) vs
    VU.unsafeFreeze vs1
  pure $ digitsToInt @Int $ reverse $ VU.toList vs2

instance Solution Day16 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (extraOps, rawInput) <- consumeExtraLeadingLines <$> getInputS
    let xs = fmap chToInt . head . lines $ rawInput
        len = length xs
        runPart1 = maybe True ("part1" `elem`) extraOps
        runPart2 = maybe True ("part2" `elem`) extraOps
    when runPart1 do
      answerShow $ digitsToInt @Int (take 8 $ iterate (onePhase len) xs !! 100)
    when runPart2 do
      let offset = digitsToInt (take 7 xs)
          cutRev = take cutLen (cycle (reverse xs))
          cutLen = len * 10000 - offset
      answerShow (performSndHalfFftRev cutRev)
