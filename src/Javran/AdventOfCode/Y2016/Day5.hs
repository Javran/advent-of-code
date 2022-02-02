{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Javran.AdventOfCode.Y2016.Day5
  (
  )
where

import Control.Monad
import qualified Crypto.Hash.MD5 as Md5
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.IntMap.Strict as IM
import Data.List
import Data.Word
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.TestExtra
import Numeric

data Day5 deriving (Generic)

{-
  Warning: strong recommend *not* doing this one,
  as it serves absolutely no purpose:
  you learn nothing except for calling an external library,
  and there's no clear method of optimization.
 -}

chHex :: Word8 -> Char
chHex v = head $ showHex v ""

extractDigit3 :: (BS.ByteString -> Bool) -> BS.ByteString -> Maybe (Char, Maybe (Int, Char))
extractDigit3 isUsable h = do
  let a = BS.index h 2
  guard $ isUsable h
  let pos = 0x0F .&. a
      part2 = do
        guard $ pos >= 0 && pos <= 7
        pure (fromIntegral pos, chHex (unsafeShiftR (BS.index h 3) 4))
  pure (chHex pos, part2)

searchSpace :: String -> [BS.ByteString]
searchSpace p = fmap gen [0 :: Int ..]
  where
    prefix = Md5.start (BSC.pack p)
    gen i = Md5.finalize $ Md5.update prefix $ BSC.pack (show i)

buildCode :: [Int] -> IM.IntMap a -> [(Int, a)] -> [a]
buildCode missing assigned ~((pos, ch) : xs)
  | null missing = IM.elems assigned
  | pos `elem` missing = buildCode (delete pos missing) (IM.insert pos ch assigned) xs
  | otherwise = buildCode missing assigned xs

{-
  For tests, relax the condition to just two prefixing zeroes.
 -}
isHashUsable :: Bool -> BS.ByteString -> Bool
isHashUsable relaxedForTest h =
  if relaxedForTest
    then BS.index h 0 == 0
    else BS.index h 0 == 0 && BS.index h 1 == 0 && (0xF0 .&. BS.index h 2) == 0

instance Solution Day5 where
  solutionRun _ SolutionContext {getInputS, answerS} = do
    (ex, rawInput) <- consumeExtra getInputS
    let inp = head . lines $ rawInput
        ss = searchSpace inp
        relaxed = isJust ex
        isUsable = isHashUsable relaxed
        usables = mapMaybe (extractDigit3 isUsable) ss
    answerS $ take 8 $ fmap fst usables
    answerS $ buildCode [0 .. 7] IM.empty (mapMaybe snd usables)
