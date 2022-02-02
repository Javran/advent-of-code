{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Javran.AdventOfCode.Y2016.Day14
  ( splitHighLow
  )
where

{-
  Warning: strongly recommend *not* doing.
  It's ironic that this puzzle talks about "Security through obscurity"
  while doing the exact same thing - "difficulty through obscurity",
  putting everything through with MD5 or any other digest algorithm,
  and congrats you have a new puzzle.

  I'm very glad that this is the last year that I see this kind of puzzle.
 -}

import Control.Monad
import qualified Crypto.Hash.MD5 as Md5
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char
import Data.List
import Data.List.Split hiding (sepBy)
import Data.Monoid
import Data.Semigroup
import Data.Word
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.TestExtra

data Day14 deriving (Generic)

splitHighLow :: Word8 -> (Word8, Word8)
splitHighLow v = (unsafeShiftR v 4, v .&. 0xF)

searchSpace :: String -> [BS.ByteString]
searchSpace salt = fmap gen [0 :: Int ..]
  where
    prefix = Md5.start (BSC.pack salt)
    gen i = Md5.finalize $ Md5.update prefix $ BSC.pack (show i)

strForm :: Word8 -> [Word8]
strForm inp = [toCh hi, toCh lo]
  where
    toCh :: Word8 -> Word8
    toCh v =
      if v < 10
        then v + fromIntegral (ord '0')
        else v + (fromIntegral (ord 'a') - 10)
    (hi, lo) = splitHighLow inp

rehash :: BS.ByteString -> BS.ByteString
rehash = Md5.hash . BS.pack . concatMap strForm . BS.unpack

unpackWord4 :: BS.ByteString -> [Word8]
unpackWord4 = concatMap f . BS.unpack
  where
    f v =
      let (hi, lo) = splitHighLow v in [hi, lo]

isKey :: [(BS.ByteString, Word16)] -> Bool
isKey ~((hd, _) : tl) = isJust do
  let threeOfAKind ~[a, b, c] = a <$ guard (a == b && b == c)
  targetNum : _ <- pure $ mapMaybe threeOfAKind $ divvy 3 1 $ unpackWord4 hd
  guard $ any ((\s -> testBit s (fromIntegral targetNum)) . snd) (take 1000 tl)

{-
  This is to pre-process five-of-a-kind information.
 -}
fives :: BS.ByteString -> Word16
fives =
  foldr (\i r -> r .|. unsafeShiftL 1 (fromIntegral i)) 0
    . mapMaybe five
    . divvy 5 1
    . unpackWord4
  where
    five ~(x : xs) = x <$ guard (all (== x) xs)

solve :: Int -> [BS.ByteString] -> Int
solve findNth ss = fst $ keys !! (findNth - 1)
  where
    ss' = fmap (\v -> (v, fives v)) ss
    keys = filter (isKey . snd) (zip [0 ..] $ tails ss')

instance Solution Day14 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (extraOps, rawInput) <- consumeExtra getInputS
    let [salt] = lines rawInput
        (findNth, rehashTimes) = singleLineExtra (64, 2016 :: Int) extraOps
        Endo rehashN = stimes rehashTimes (Endo rehash)
        ss = searchSpace salt
        ss2 = fmap rehashN ss
    answerShow (solve findNth ss)
    answerShow (solve findNth ss2)
