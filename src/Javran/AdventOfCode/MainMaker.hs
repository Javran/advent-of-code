{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Javran.AdventOfCode.MainMaker
  ( runSomeSolution
  , mkYearlyMain
  , exampleRawInputRelativePath
  , editExample
  , runSolutionWithExampleAndWriteExpect
  )
where

import Control.Monad
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Javran.AdventOfCode.Cli.TestdataDigest
import Javran.AdventOfCode.Infra
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import qualified Turtle.Bytes as TBytes

editExample :: Int -> Int -> IO ()
editExample yyyy dd = do
  mEditorCmd <- lookupEnv "EDITOR"
  projectHome <- getEnv "PROJECT_HOME"
  let subPath = exampleRawInputRelativePath yyyy dd
      exampleInputFileName = "example.input.txt"
      exampleDir = projectHome </> subPath
  createDirectoryIfMissing True exampleDir
  let exampleFp = exampleDir </> exampleInputFileName
  -- ensure that the file exists.
  do
    e <- doesFileExist exampleFp
    unless e $ writeFile exampleFp ""
  case mEditorCmd of
    Just editorCmd | not (all isSpace editorCmd) -> do
      ec <- TBytes.proc (T.pack editorCmd) [T.pack $ exampleDir </> exampleInputFileName] ""
      when
        (ec == ExitSuccess)
        performTestdataSpecHashSync
      exitWith ec
    _ ->
      do
        print mEditorCmd
        putStrLn "EDITOR is empty, please edit the file manually:"
        putStrLn exampleFp

{-
  TODO: expect files are not used for now - in future might use it as unit test.
  TODO: write-expect should scan all examples and write to them.
 -}
runSolutionWithExampleAndWriteExpect :: forall p sol. Solution sol => p sol -> IO ()
runSolutionWithExampleAndWriteExpect p = do
  projectHome <- getEnv "PROJECT_HOME"
  let (yyyy, dd) = solutionIndex p
  actualOutput <- runSolutionWithExampleInput p True
  let fpTarget = projectHome </> subPath </> "example.expect.txt"
      subPath = exampleRawInputRelativePath yyyy dd
  T.writeFile fpTarget actualOutput
  putStrLn $ "Written to: " <> fpTarget
  performTestdataSpecHashSync

runSomeSolution :: SomeSolution -> String -> IO ()
runSomeSolution (SomeSolution s) cmdHelpPrefix = do
  args <- getArgs
  let runLogin = runSolutionWithLoginInput s True >>= T.putStr
      runExample = runSolutionWithExampleInput s True >>= T.putStr
  case args of
    [] ->
      runLogin
    ["l"] -> runLogin
    ["login"] -> runLogin
    ["e"] -> runExample
    ["example"] -> runExample
    ["edit-example"] ->
      let (yyyy, dd) = solutionIndex s
       in editExample yyyy dd
    ["write-expect"] ->
      runSolutionWithExampleAndWriteExpect s
    _ ->
      die $ cmdHelpPrefix <> "[e|example|l|login|edit-example|write-expect]"

mkYearlyMain :: Int -> [SomeSolution] -> String -> IO ()
mkYearlyMain year collectedSolutions = yearlyMain
  where
    solutionRunners = mk <$> collectedSolutions
      where
        mk ss@(SomeSolution s) = case solutionIndex s of
          (yyyy, dd)
            | yyyy == year ->
              (show dd, runSomeSolution ss)
          solInd -> error $ "Invalid solution index: " <> show solInd

    yearlyMain :: String -> IO ()
    yearlyMain cmdHelpPrefix = do
      dispatchToSubCmds
        cmdHelpPrefix
        solutionRunners
