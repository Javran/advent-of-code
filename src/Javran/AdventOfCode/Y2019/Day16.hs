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
onePhase len xs = fmap (\i -> lastDigit $ sum $ zipWith (*) xs (genSeq i)) [1 .. len]

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
