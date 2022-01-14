{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Javran.AdventOfCode.Y2017.Day15
  (
  )
where

import Data.Bits
import Data.Word
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day15 deriving (Generic)

inputP :: ReadP (Word32, Word32)
inputP = do
  let nl = char '\n'
  a <- between (string "Generator A starts with ") nl decimal1P
  b <- between (string "Generator B starts with ") nl decimal1P
  pure (a, b)

-- https://en.wikipedia.org/wiki/Lehmer_random_number_generator#Sample_C99_code
parkMiller :: Word64 -> Word32 -> Word32
parkMiller factor inp = x1
  where
    prod :: Word64
    prod = fromIntegral inp * factor
    x0, x1 :: Word32
    x0 = fromIntegral (prod .&. 0x7FFF_FFFF) + fromIntegral (shift prod (-31))
    x1 = (x0 .&. 0x7FFF_FFFF) + shift x0 (-31)

genSeq :: Word64 -> Word32 -> [Word32]
genSeq factor seed = tail $ iterate (parkMiller factor) seed
{-# INLINE genSeq #-}

instance Solution Day15 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (seedA, seedB) <- consumeOrDie inputP <$> getInputS
    let judge (a, b) = (a .&. 0xFFFF) == (b .&. 0xFFFF)
    do
      let xs = genSeq 16807 seedA
          ys = genSeq 48271 seedB
      answerShow $ countLength judge $ take 40_000_000 $ zip xs ys
    do
      {-
        sharing would hurt performance in this case: holding on to the existing sequence
        will keep things in memory while a large amount of them are no longer needed
        (due to added constraints about `multiple of x`)
       -}
      let xs' = filter (\v -> v .&. 3 == 0) $ genSeq 16807 seedA
          ys' = filter (\v -> v .&. 7 == 0) $ genSeq 48271 seedB
      answerShow $ countLength judge $ take 5_000_000 $ zip xs' ys'
