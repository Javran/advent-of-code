{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2020Spec where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.IO as T
import Javran.AdventOfCode.Infra
import Javran.AdventOfCode.Tester
import Javran.AdventOfCode.Y2020.Main
import Test.Hspec

spec :: Spec
spec = forM_ allSolutions $ \ss@(SomeSolution s) -> do
  let idx@(_yyyy, dd) = solutionIndex s
  describe ("Day" <> show dd) $ do
    specify "testdata" $ do
      ts <- liftIO $ scanForSomeSolution ss
      forM_ ts $ \TestDataInfo {tag, inputFilePath, mExpectFilePath} -> do
        (out, mExpectOut) <- liftIO $ do
          putStrLn $ "++++ index: " <> show idx <> ", tag: " <> tag
          output <- runSolutionWithInputGetter s (\_ _ -> BSL.readFile inputFilePath)
          mExpectContent <- case mExpectFilePath of
            Nothing -> do
              putStrLn "*.expect.txt not found"
              pure Nothing
            Just ePath -> Just <$> T.readFile ePath
          putStrLn $ "---- index: " <> show idx <> ", tag: " <> tag
          pure (output, mExpectContent)
        case mExpectOut of
          Nothing -> pending
          Just e -> out `shouldBe` e
