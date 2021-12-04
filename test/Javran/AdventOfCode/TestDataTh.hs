{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Javran.AdventOfCode.TestDataTh
  ( collectTests
  , mkSpecFromStructuredTestData
  )
where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as T
import Javran.AdventOfCode.Infra
import Javran.AdventOfCode.Tester
import qualified Javran.AdventOfCode.Y2020.Main as Y2020
import qualified Javran.AdventOfCode.Y2021.Main as Y2021
import Language.Haskell.TH
import Test.Hspec hiding (runIO)

{-
  TODO: hls will crash on test/, not sure why.

  probably related: https://github.com/haskell/haskell-language-server/issues/2314
 -}

getSolution :: Int -> Int -> SomeSolution
getSolution year day = allSolutions M.! (year, day)
  where
    allSolutions = M.fromList $ do
      someSol@(SomeSolution s) <- Y2020.allSolutions <> Y2021.allSolutions
      let sInd = solutionIndex s
      pure (sInd, someSol)

collectTests :: Q Exp
collectTests = do
  d <- runIO scanTestData
  [|d :: StructuredTestData|]

mkSpecFromStructuredTestData :: StructuredTestData -> Spec
mkSpecFromStructuredTestData = mapM_ (uncurry handleYear)
  where
    handleYear :: Int -> [(Int, [TestDataInfo])] -> Spec
    handleYear year =
      describe ('Y' : show year)
        . mapM_ (uncurry handleDay)
      where
        handleDay :: Int -> [TestDataInfo] -> Spec
        handleDay day tds =
          describe ("Day" <> show day) $
            forM_ tds $
              \TestDataInfo {tag, inputFilePath, mExpectFilePath} -> do
                specify tag $ do
                  case getSolution year day of
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
