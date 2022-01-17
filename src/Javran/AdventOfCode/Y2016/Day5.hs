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
import Numeric

data Day5 deriving (Generic)

chHex :: Word8 -> Char
chHex v = head $ showHex v ""

extractDigit3 :: BS.ByteString -> Maybe (Char, Maybe (Int, Char))
extractDigit3 h = do
  let a = BS.index h 2
  guard $ BS.index h 0 == 0 && BS.index h 1 == 0 && (0xF0 .&. a) == 0
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

instance Solution Day5 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerS} = do
    inp <- head . lines <$> getInputS
    let ss = searchSpace inp
        usables = mapMaybe extractDigit3 ss
    answerS $ take 8 $ fmap fst usables
    answerS $ buildCode [0 .. 7] IM.empty (mapMaybe snd usables)
