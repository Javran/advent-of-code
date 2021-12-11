{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Javran.AdventOfCode.Cli.TestdataDigest
  ( computeTestdataDirDigestTextRep
  , performTestdataSpecHashSync
  )
where

import qualified Control.Foldl as Foldl
import Control.Monad
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List
import Data.Text.Encoding
import Data.Word
import qualified Filesystem.Path.CurrentOS as FP
import Javran.AdventOfCode.Infra
import Javran.AdventOfCode.Prelude
import Numeric
import System.Environment
import System.FilePath.Posix
import Turtle.Prelude hiding (sort)
import Turtle.Shell

digestTextRep :: BS.ByteString -> String
digestTextRep = ($ []) . foldr (\i f -> paddedHex i . f) id . BS.unpack
  where
    paddedHex :: Word8 -> ShowS
    paddedHex v =
      case showHex v [] of
        [c] -> (\rs -> '0' : c : rs)
        ys@[_, _] -> (ys <>)
        _ -> unreachable

computeTestdataDirDigestTextRep :: IO String
computeTestdataDirDigestTextRep = do
  {-
    Don't load from StockData for scanning:

    - could be the case that the binary is ran without involving Cabal,
      in which case new testdata stucture won't be reflected there.

    - command line expects PROJECT_HOME to present anyway

   -}
  projectHome <- getEnv "PROJECT_HOME"
  dirs <- reduce (fmap sort Foldl.list) do
    let testdataPath = FP.decodeString projectHome FP.</> "data" FP.</> "testdata"
    -- let it produce relative path, as what is outside of projectHome should never matter.
    pushd testdataPath
    fp <- lstree testdataPath
    st <- stat fp
    guard $ isRegularFile st
    {-
      Note: this encoding is not OS-independent as path separator varies,
      for now I don't think this is a problem worth working on.
     -}
    pure (either encodeUtf8 encodeUtf8 $ FP.toText fp)
  let payload = BSL.intercalate (BSL.singleton 0) $ fmap BSL.fromStrict dirs
      digest = SHA256.hashlazy payload
  pure $ digestTextRep digest

performTestdataSpecHashSync :: IO ()
performTestdataSpecHashSync = do
  projectHome <- getEnv "PROJECT_HOME"
  let fp = projectHome </> "test" </> "Javran" </> "AdventOfCode" </> "TestdataSpec.hs"
  digest <- computeTestdataDirDigestTextRep
  let digestContent = ["hashForForceRecompliation = \"" <> digest <> "\""]
      extractSecCb =
        (\prevSec bm sec em postSec ->
           if sec == digestContent
             then -- no need for editing, nothing is changed.
               (sec, Just False)
             else
               ( prevSec <> [bm] <> digestContent <> [em] <> postSec
               , Just True
               ))

  mayEditFileWithSpecialSection
    fp
    "TestdataSpec: "
    "-- FORCE_RECOMP_HASH_BEGIN"
    "-- FORCE_RECOMP_HASH_END"
    extractSecCb
