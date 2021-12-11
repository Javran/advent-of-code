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
import Data.Maybe
import qualified Data.Text.IO as T
import Javran.AdventOfCode.Infra
import Javran.AdventOfCode.Solutions (getSolution)
import Javran.AdventOfCode.Testdata
import Language.Haskell.TH
import Test.Hspec hiding (runIO)

{-
  Note: haskell-language-server might segfault, likely due to the presence of
  those TH machinery.

  According to https://github.com/haskell/haskell-language-server/issues/2314,
  the current workaround is to build a dynamically linked binary,
  which at least solves the problem from my side.
 -}
collectTests :: Q Exp
collectTests = do
  d <- runIO $ scanTestdata "."
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
                          runSolutionWithInputGetter s (\_ _ -> BSL.readFile inputFilePath) False Nothing
                      case mExpectFilePath of
                        Nothing -> do
                          pendingWith "*.expect.txt not found"
                        Just ePath -> do
                          expectContent <- liftIO $ T.readFile ePath
                          output `shouldBe` expectContent
