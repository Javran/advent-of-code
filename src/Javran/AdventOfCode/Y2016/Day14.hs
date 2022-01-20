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

module Javran.AdventOfCode.Y2016.Day14
  (
  )
where

{-
  It's ironic that this puzzle talks about "Security through obscurity"
  while doing the exact same thing - "difficulty through obscurity",
  with this MD5 bs.

  I'm very glad that this is the last year that I see this kind of puzzle.
 -}

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import qualified Crypto.Hash.MD5 as Md5
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
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
import qualified Data.Vector as V
import Data.Word
import Debug.Trace
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)
import Text.Printf

data Day14 deriving (Generic)

splitHighLow :: Word8 -> (Word8, Word8)
splitHighLow v = (unsafeShiftR v 4, v .&. 0xF)

searchSpace :: String -> [BS.ByteString]
searchSpace salt = fmap gen [0 :: Int ..]
  where
    prefix = Md5.start (BSC.pack salt)
    gen i = Md5.finalize $ Md5.update prefix $ BSC.pack (show i)

strForm :: Word8 -> [Word8]
strForm inp = [toCh hi , toCh lo]
  where
    toCh :: Word8 -> Word8
    toCh v =
      if v < 10
        then v + fromIntegral (ord '0')
        else v + (fromIntegral (ord 'a') - 10)
    (hi, lo) = splitHighLow inp

rehash :: BS.ByteString -> BS.ByteString
rehash = Md5.hash . BS.pack . concatMap strForm . BS.unpack

rehash2016 :: BS.ByteString -> BS.ByteString
rehash2016 = appEndo $ stimes (2016 :: Int) (Endo rehash)

unpackWord4 :: BS.ByteString -> [Word8]
unpackWord4 = concatMap f . BS.unpack
  where
    f v =
      let (hi, lo) = splitHighLow v in [hi, lo]

isKey :: [BS.ByteString] -> Bool
isKey ~(hd : tl) = isJust do
  let threeOfAKind ~[a, b, c] = a <$ guard (a == b && b == c)
  targetNum : _ <- pure $ mapMaybe threeOfAKind $ divvy 3 1 $ unpackWord4 hd
  guard $ any (any (== replicate 5 targetNum) . divvy 5 1 . unpackWord4) (take 1000 tl)

solve :: [BS.ByteString] -> Int
solve ss = fst $ keys !! 63
  where
    keys = filter (isKey . snd) (zip [0 ..] $ tails ss)

instance Solution Day14 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    salt <- head . lines <$> getInputS
    do
      let ss = searchSpace salt
      answerShow (solve ss)
    do
      let ss = fmap rehash2016 $ searchSpace salt
      answerShow (solve ss)
