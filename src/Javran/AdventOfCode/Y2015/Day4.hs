{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2015.Day4
  (
  )
where

import qualified Crypto.Hash.MD5 as Md5
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.TestExtra

data Day4 deriving (Generic)

searchSpace :: String -> [(Int, BS.ByteString)]
searchSpace p = fmap gen [1 :: Int ..]
  where
    prefix = Md5.start (BSC.pack p)
    gen i = (i, Md5.finalize $ Md5.update prefix $ BSC.pack (show i))

{-
  For tests, relax the condition to just two prefixing zeroes.
 -}
isHashUsable :: Bool -> BS.ByteString -> Bool
isHashUsable relaxedForTest h =
  if relaxedForTest
    then BS.index h 0 == 0
    else BS.index h 0 == 0 && BS.index h 1 == 0 && (0xF0 .&. BS.index h 2) == 0

isHashUsable2 :: Bool -> BS.ByteString -> Bool
isHashUsable2 relaxedForTest h =
  if relaxedForTest
    then BS.index h 0 == 0 && BS.index h 1 == 0
    else BS.index h 0 == 0 && BS.index h 1 == 0 && BS.index h 2 == 0

instance Solution Day4 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (extraOps, rawInput) <- consumeExtra getInputS
    let salt = head . lines $ rawInput
        testing = isJust extraOps
    do
      let (ans, _) : _ =
            filter (isHashUsable testing . snd) $ searchSpace salt
      answerShow ans
    do
      let (ans, _) : _ =
            filter (isHashUsable2 testing . snd) $ searchSpace salt
      answerShow ans
