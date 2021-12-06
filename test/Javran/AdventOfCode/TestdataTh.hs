{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Javran.AdventOfCode.Solutions (getSolution)
import Javran.AdventOfCode.Infra
import Javran.AdventOfCode.Tester
import Language.Haskell.TH
import Test.Hspec hiding (runIO)

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
                          runSolutionWithInputGetter s (\_ _ -> BSL.readFile inputFilePath)
                      case mExpectFilePath of
                        Nothing -> do
                          pendingWith "*.expect.txt not found"
                        Just ePath -> do
                          expectContent <- liftIO $ T.readFile ePath
                          output `shouldBe` expectContent
