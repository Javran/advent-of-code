{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Tester
  ( TestDataInfo (..)
  , scanForSomeSolution
  , StructuredTestData
  , scanTestData
  , subCommand
  )
where

import Control.Monad
import Data.List
import Data.Maybe
import Javran.AdventOfCode.Infra
import Javran.AdventOfCode.Prelude
import qualified Paths_advent_of_code as StockData
import System.Directory
import System.FilePath.Posix
import Text.ParserCombinators.ReadP
import Language.Haskell.TH.Syntax

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

subCommand :: String -> IO ()
subCommand _ = scanTestData >>= mapM_ print
