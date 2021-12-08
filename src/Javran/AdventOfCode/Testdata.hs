{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Testdata
  ( StructuredTestdata
  , scanTestdata
  , TestdataInfo(..)
  , scanForSolution
  )
where

import Control.Monad
import Data.List
import Data.Maybe
import Javran.AdventOfCode.Infra
import Language.Haskell.TH.Syntax
import System.Directory
import System.FilePath.Posix
import Text.ParserCombinators.ReadP

data TestdataInfo = TestdataInfo
  { tag :: String
  , inputFilePath :: FilePath
  , mExpectFilePath :: Maybe FilePath
  }
  deriving (Show, Lift)

{-
  parses something like "foo.input.txt" as "foo",
  verifying the ".input.txt" part but drops it.
 -}
testInputFileNameP :: ReadP String
testInputFileNameP = get `manyTill` string ".input.txt"

scanForSolution :: FilePath -> (Int, Int) -> IO [TestdataInfo]
scanForSolution baseDir (yyyy, dd) = do
  let targetPath = baseDir </> exampleRawInputRelativePath yyyy dd
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
      TestdataInfo
        { tag
        , inputFilePath = targetPath </> (tag <> ".input.txt")
        , mExpectFilePath = do
            guard $ expectFileName `elem` allFiles
            pure $ targetPath </> expectFileName
        }

type StructuredTestdata = [(Int, [(Int, [TestdataInfo])])]

scanTestdata :: FilePath -> IO StructuredTestdata
scanTestdata baseDir = do
  let yearPath = baseDir </> "data" </> "testdata"
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
      forM days $ \day -> (day,) <$> scanForSolution baseDir (year, day)
