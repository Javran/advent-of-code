{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Tester
  ( TestDataInfo (..)
  , scanForSomeSolution
  , StructuredTestData
  , scanTestData
  , subCommand
  , computeTestdataDirDigestTextRep
  )
where

import qualified Control.Foldl as Foldl
import Control.Monad
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List
import Data.Maybe
import Data.Text.Encoding
import Data.Word
import qualified Filesystem.Path.CurrentOS as FP
import Javran.AdventOfCode.Infra
import Javran.AdventOfCode.Prelude
import Language.Haskell.TH.Syntax
import Numeric
import qualified Paths_advent_of_code as StockData
import System.Directory
import System.Environment
import System.FilePath.Posix
import Text.ParserCombinators.ReadP
import Turtle.Prelude hiding (sort)
import Turtle.Shell

data TestDataInfo = TestDataInfo
  { tag :: String
  , inputFilePath :: FilePath
  , mExpectFilePath :: Maybe FilePath
  }
  deriving (Show, Lift)

testInputFileNameP :: ReadP String
testInputFileNameP = get `manyTill` string ".input.txt"

scanForSolution :: (Int, Int) -> IO [TestDataInfo]
scanForSolution (yyyy, dd) = do
  dataDir <- StockData.getDataDir
  let targetPath = dataDir </> exampleRawInputRelativePath yyyy dd
  allFiles <-
    listDirectory targetPath
      >>= filterM
        (\fn ->
           if ".txt" `isSuffixOf` fn
             then doesFileExist (targetPath </> fn)
             else pure False)
  let allInputTestTags = mapMaybe (consumeAllWithReadP testInputFileNameP) allFiles
  pure $ do
    tag <- allInputTestTags
    let expectFileName = tag <> ".expect.txt"
    pure $
      TestDataInfo
        { tag
        , inputFilePath = targetPath </> (tag <> ".input.txt")
        , mExpectFilePath = do
            guard $ expectFileName `elem` allFiles
            pure $ targetPath </> expectFileName
        }

scanForSomeSolution :: SomeSolution -> IO [TestDataInfo]
scanForSomeSolution (SomeSolution s) = scanForSolution (solutionIndex s)

type StructuredTestData = [(Int, [(Int, [TestDataInfo])])]

scanTestData :: IO StructuredTestData
scanTestData = do
  dataDir <- StockData.getDataDir
  let yearPath = dataDir </> "data" </> "testdata"
  yearsRaw <-
    listDirectory yearPath
      >>= filterM (\p -> doesDirectoryExist (yearPath </> p))
  let years = sort $ mapMaybe (consumeAllWithReadP @Int decimal1P) yearsRaw
  forM years $ \year ->
    (year,) <$> do
      let dayPath = yearPath </> show year </> "day"
      daysRaw <-
        listDirectory dayPath
          >>= filterM (\p -> doesDirectoryExist (dayPath </> p))
      let days = sort $ mapMaybe (consumeAllWithReadP @Int decimal1P) daysRaw
      forM days $ \day -> (day,) <$> scanForSolution (year, day)

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
    pure (either encodeUtf8 encodeUtf8 $ FP.toText fp)
  let payload = BSL.intercalate (BSL.singleton 0) $ fmap BSL.fromStrict dirs
      digest = SHA256.hashlazy payload
  pure $ digestTextRep digest

subCommand :: String -> IO ()
subCommand _ = computeTestdataDirDigestTextRep >>= putStrLn
