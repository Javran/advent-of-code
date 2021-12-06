{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.TestdataTh
  ( collectTests
  , mkSpecFromStructuredTestdata
  )
where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BSL
import Data.List
import Data.Maybe
import qualified Data.Text.IO as T
import Javran.AdventOfCode.Infra
import Javran.AdventOfCode.Solutions (getSolution)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Paths_advent_of_code as StockData
import System.Directory
import System.FilePath.Posix
import Test.Hspec hiding (runIO)
import Text.ParserCombinators.ReadP

data TestdataInfo = TestdataInfo
  { tag :: String
  , inputFilePath :: FilePath
  , mExpectFilePath :: Maybe FilePath
  }
  deriving (Show, Lift)

testInputFileNameP :: ReadP String
testInputFileNameP = get `manyTill` string ".input.txt"

scanForSolution :: (Int, Int) -> IO [TestdataInfo]
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
      TestdataInfo
        { tag
        , inputFilePath = targetPath </> (tag <> ".input.txt")
        , mExpectFilePath = do
            guard $ expectFileName `elem` allFiles
            pure $ targetPath </> expectFileName
        }

type StructuredTestdata = [(Int, [(Int, [TestdataInfo])])]

scanTestdata :: IO StructuredTestdata
scanTestdata = do
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

{-
  TODO: hls will crash on test/, not sure why.

  probably related: https://github.com/haskell/haskell-language-server/issues/2314
 -}
collectTests :: Q Exp
collectTests = do
  d <- runIO scanTestdata
  [|d :: StructuredTestdata|]

mkSpecFromStructuredTestdata :: StructuredTestdata -> Spec
mkSpecFromStructuredTestdata = mapM_ (uncurry handleYear)
  where
    handleYear :: Int -> [(Int, [TestdataInfo])] -> Spec
    handleYear year =
      describe ('Y' : show year)
        . mapM_ (uncurry handleDay)
      where
        handleDay :: Int -> [TestdataInfo] -> Spec
        handleDay day tds =
          describe ("Day" <> show day) $
            forM_ tds $
              \TestdataInfo {tag, inputFilePath, mExpectFilePath} -> do
                specify tag $ do
                  case fromJust $ getSolution year day of
                    SomeSolution s -> do
                      output <-
                        liftIO $
                          runSolutionWithInputGetter s (\_ _ -> BSL.readFile inputFilePath) False
                      case mExpectFilePath of
                        Nothing -> do
                          pendingWith "*.expect.txt not found"
                        Just ePath -> do
                          expectContent <- liftIO $ T.readFile ePath
                          output `shouldBe` expectContent
