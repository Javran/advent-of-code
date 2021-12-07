{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Javran.AdventOfCode.MainMaker
  ( runSomeSolution
  , exampleRawInputRelativePath
  , runSolutionWithExampleAndWriteExpect
  )
where

import Control.Monad
import qualified Data.Text.IO as T
import Javran.AdventOfCode.Cli.TestdataDigest
import Javran.AdventOfCode.Infra
import System.Console.Terminfo
import System.Environment
import System.Exit
import System.FilePath.Posix
import Javran.AdventOfCode.Cli.New
import Javran.AdventOfCode.Cli.EditExample

{-
  TODO: expect files are not used for now - in future might use it as unit test.
  TODO: write-expect should scan all examples and write to them.
 -}
runSolutionWithExampleAndWriteExpect :: forall p sol. Solution sol => p sol -> Maybe Terminal -> IO ()
runSolutionWithExampleAndWriteExpect p mTerm = do
  projectHome <- getEnv "PROJECT_HOME"
  let (yyyy, dd) = solutionIndex p
  actualOutput <- runSolutionWithExampleInput p True mTerm
  let fpTarget = projectHome </> subPath </> "example.expect.txt"
      subPath = exampleRawInputRelativePath yyyy dd
  T.writeFile fpTarget actualOutput
  putStrLn $ "Written to: " <> fpTarget
  -- TODO: also update README?
  performTestdataSpecHashSync

runSomeSolution :: SomeSolution -> SubCmdContext -> IO ()
runSomeSolution (SomeSolution s) SubCmdContext {cmdHelpPrefix, mTerm} = do
  args <- getArgs
  let runLogin = void $ runSolutionWithLoginInput s True mTerm
      runExample = void $ runSolutionWithExampleInput s True mTerm
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
      runSolutionWithExampleAndWriteExpect s mTerm
    ["new"] ->
      let (yyyy, dd) = solutionIndex s
       in newCommandForYearDay yyyy dd
    _ ->
      die $ cmdHelpPrefix <> "[e|example|l|login|edit-example|write-expect|new]"
