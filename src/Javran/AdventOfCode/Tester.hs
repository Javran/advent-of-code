{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Tester
  ( TestDataInfo (..)
  , scanForSomeSolution
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

data TestDataInfo = TestDataInfo
  { tag :: String
  , inputFilePath :: FilePath
  , mExpectFilePath :: Maybe FilePath
  }

testInputFileNameP :: ReadP String
testInputFileNameP = get `manyTill` string ".input.txt"

scanForSomeSolution :: SomeSolution -> IO [TestDataInfo]
scanForSomeSolution (SomeSolution s) = do
  dataDir <- StockData.getDataDir
  let (yyyy, dd) = solutionIndex s
      targetPath = dataDir </> exampleRawInputRelativePath yyyy dd
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
