{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Javran.AdventOfCode.Cli.SolutionCommand.SolutionMain
  ( runMainWith
  )
where

import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.String
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Javran.AdventOfCode.Cli.EditExample
import Javran.AdventOfCode.Cli.New
import Javran.AdventOfCode.Cli.ProgressReport
import Javran.AdventOfCode.Cli.TestdataDigest
import Javran.AdventOfCode.Infra
import Javran.AdventOfCode.Network (submitAnswer)
import Javran.AdventOfCode.Solutions
import Javran.AdventOfCode.Testdata (TestdataInfo (..), scanForSolution)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.IO
import Text.Printf
import qualified Turtle

{-
  TODO:

  - a function to list all examples
  - function to attach EXAMPLE_EXTRA_XXX section to an example
  - getInputAndExtraS as part of a solution context,
    that seperates extra stuff in one go.

 -}

data ExampleName
  = ExName String
  | -- | (only valid for running) special name for running all examples
    ExAll

data Command
  = CmdRunLogin
  | CmdRunExample ExampleName
  | CmdEditExample ExampleName
  | CmdWriteExampleExpect
  | CmdNewSolution
  | CmdSubmit Int String
  | CmdTest

exampleNameToName :: ExampleName -> FilePath
exampleNameToName = \case
  ExName n -> n
  _ -> error "unsupported"

getExampleInputPath :: Int -> Int -> ExampleName -> FilePath
getExampleInputPath year day = \case
  e@ExName {} -> subPath </> (exampleNameToName e <> ".input.txt")
  ExAll -> todo
  where
    subPath = "data" </> "testdata" </> show year </> "day" </> show day

runSolutionWithExampleAndWriteExpect :: forall p sol. Solution sol => p sol -> IO ()
runSolutionWithExampleAndWriteExpect p = do
  projectHome <- getEnv "PROJECT_HOME"
  let solInd@(yyyy, dd) = solutionIndex p
  tis <- scanForSolution projectHome solInd
  forM_ tis $ \TestdataInfo {inputFilePath, tag} -> do
    putStrLn $ "Processing tag: " <> tag <> " ..."
    actualOutput <- runSolutionWithInputGetter p (\_ _ -> BSL.readFile inputFilePath) False Nothing
    let fpTarget = projectHome </> subPath </> (tag <> ".expect.txt")
        subPath = exampleRawInputRelativePath yyyy dd
    mExistingOutput <- do
      e <- doesFileExist fpTarget
      if e
        then Just <$> T.readFile fpTarget
        else pure Nothing
    if mExistingOutput == Just actualOutput
      then putStrLn $ "Unchanged: " <> fpTarget
      else do
        T.writeFile fpTarget actualOutput
        putStrLn $ "Written to: " <> fpTarget
  performTestdataSpecHashSync
  performReadmeProgressSync

parseExampleName :: String -> Either String ExampleName
parseExampleName xs
  | null xs = Left "empty name"
  | xs == "all" = Right ExAll
  | _ : _ <- xs = pure $ ExName xs
  | otherwise = Left $ "Unrecognized: " <> xs

parseArgs :: [String] -> Either String Command
parseArgs = \case
  [] -> Right CmdRunLogin
  cmd : args ->
    if
        | cmd `elem` ["l", "login"] -> CmdRunLogin <$ expectNoExtra args
        | cmd `elem` ["e", "example"] ->
          CmdRunExample <$> do
            mx <- atMostOneExtra args
            maybe (pure defExample) parseExampleName mx
        | cmd == "edit-example" ->
          CmdEditExample <$> do
            mx <- atMostOneExtra args
            maybe (pure defExample) parseExampleName mx
        | cmd == "write-expect" -> CmdWriteExampleExpect <$ expectNoExtra args
        | cmd == "new" -> CmdNewSolution <$ expectNoExtra args
        | cmd == "submit" -> do
          -- TODO: be more rigid on this.
          let [whichRaw, answer] = args
          pure $ CmdSubmit (read whichRaw) answer
        | cmd == "test" ->
          CmdTest <$ expectNoExtra args
        | otherwise -> Left $ "Unrecognized: " <> unwords (cmd : args)
  where
    defExample = ExName "example"
    atMostOneExtra = \case
      [] -> pure Nothing
      [x] -> pure (Just x)
      xs -> Left $ "Expect at most one extra arg: " <> unwords xs
    expectNoExtra = \case
      [] -> pure ()
      xs@(_ : _) -> Left $ "Extra args not allowed: " <> unwords xs

{-
  This is the handler responsible for running subcommands specific to one solution.

  e.g. commands like `<prog> <year> <day> <action> <args>...` are eventually dispatched

  to here with `<action> <args>...` passed as function parameters.
 -}
runMainWith :: SubCmdContext -> Int -> Int -> [String] -> IO ()
runMainWith SubCmdContext {cmdHelpPrefix, mTerm, manager} year day args = do
  cmd <- case parseArgs args of
    Left msg -> do
      let p = hPutStrLn stderr
          mkHelp shortHelp descs = do
            p $ cmdHelpPrefix <> shortHelp
            forM_ descs \d ->
              p $ "  " <> d
      p msg
      mkHelp "[l | login]" ["Runs solution with login data."]
      mkHelp "<e | example> [example name]" ["Runs solution with an example input."]
      mkHelp "edit-example [example name]" ["Touches an example and opens EDITOR."]
      mkHelp "write-expect" ["Writes *.expect by running all examples, also updates README.md."]
      mkHelp "test" ["Run `stack test` filtered to this specific solution."]
      mkHelp
        "new"
        [ "Creates new solution file for a problem."
        , "This command should be idempotent that it won't overwrite pre-existing files."
        ]
      exitFailure
    Right v -> pure v
  case cmd of
    CmdNewSolution ->
      -- new is valid whether a solution exists or not.
      newCommandForYearDay year day
    _ ->
      case getSolution year day of
        Just (SomeSolution s) ->
          case cmd of
            CmdNewSolution -> unreachable
            CmdRunLogin -> void $ runSolutionWithLoginInput s manager True mTerm
            CmdRunExample e -> do
              projectHome <- getEnv "PROJECT_HOME"
              let inputFilePath = projectHome </> getExampleInputPath year day e
              void $ runSolutionWithInputGetter s (\_ _ -> BSL.readFile inputFilePath) True mTerm
            CmdEditExample e ->
              editExampleWithName year day (exampleNameToName e)
            CmdWriteExampleExpect ->
              runSolutionWithExampleAndWriteExpect s
            CmdSubmit part answer -> do
              mySession <- getEnv "ADVENT_OF_CODE_SESSION"
              results <- submitAnswer manager (BSC.pack mySession) year day part (BSC.pack answer)
              T.putStrLn . T.unwords . T.words $ mconcat results
            CmdTest -> do
              projectHome <- getEnv "PROJECT_HOME"
              let filterArg :: String
                  filterArg = printf "--match=/Y%d/Day%d/" year day
              Turtle.cd (fromString projectHome)
              exitWith =<< Turtle.proc "stack" ["test", "--ta=" <> T.pack filterArg] ""
        Nothing ->
          die "No solution available, only `new` command is accepted."
