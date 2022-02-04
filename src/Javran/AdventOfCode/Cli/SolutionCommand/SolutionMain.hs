{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Cli.SolutionCommand.SolutionMain
  ( runMainWith
  )
where

import Control.Exception
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.List
import Data.String
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Javran.AdventOfCode.Cli.EditExample
import Javran.AdventOfCode.Cli.New
import Javran.AdventOfCode.Cli.ProgressReport
import Javran.AdventOfCode.Cli.TestdataDigest
import Javran.AdventOfCode.Infra
import Javran.AdventOfCode.Network
import Javran.AdventOfCode.Solutions
import Javran.AdventOfCode.Testdata (TestdataInfo (..), scanForSolution)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.IO
import qualified System.IO.Strict
import Text.Printf
import qualified Turtle

data ExampleName
  = ExName String
  | -- | (only valid for running) special name for running all examples
    ExAll
  deriving (Show)

data Command
  = CmdRunLogin
  | CmdRunExample ExampleName
  | CmdEditExample ExampleName
  | CmdWriteExampleExpect
  | CmdNewSolution
  | CmdSubmit Int String
  | CmdTest
  | CmdAddExtra ExampleName
  | CmdListExamples
  | CmdDownload (Maybe FilePath)

getExampleInputPath :: Int -> Int -> String -> FilePath
getExampleInputPath year day n =
  subPath </> (n <> ".input.txt")
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
        | cmd == "submit" -> case args of
          [whichRaw, answer]
            | [(w, "")] <- reads whichRaw
              , w `elem` [1, 2] ->
              pure $ CmdSubmit w answer
          _ -> Left "Expected <1|2> <answer>"
        | cmd == "test" ->
          CmdTest <$ expectNoExtra args
        | cmd == "add-extra" ->
          CmdAddExtra <$> do
            mx <- atMostOneExtra args
            maybe (pure defExample) parseExampleName mx
        | cmd == "ls" -> CmdListExamples <$ expectNoExtra args
        | cmd == "download" ->
          CmdDownload <$> atMostOneExtra args
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
      mkHelp "add-extra" ["Prepend extra input fields to tests."]
      mkHelp "ls" ["List all examples."]
      mkHelp "download [dest]" ["Download login input to a specific location or stdout if left empty."]
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
              case e of
                ExName n -> do
                  let inputFilePath = projectHome </> getExampleInputPath year day n
                  void $ runSolutionWithInputGetter s (\_ _ -> BSL.readFile inputFilePath) True mTerm
                ExAll -> do
                  tis <- scanForSolution projectHome (year, day)
                  forM_ tis $ \TestdataInfo {inputFilePath, tag} -> do
                    putStrLn $ "Running with: " <> tag <> " ..."
                    catch @SomeException
                      (void $
                         runSolutionWithInputGetter
                           s
                           (\_ _ -> BSL.readFile inputFilePath)
                           True
                           mTerm)
                      $ \exc ->
                        putStrLn $ "Exception caught: " <> displayException exc
            CmdEditExample e ->
              case e of
                ExName n ->
                  editExampleWithName year day n
                ExAll ->
                  die "edit-example does not support `all`."
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
            CmdAddExtra e -> do
              projectHome <- getEnv "PROJECT_HOME"
              tagAndInputFilePaths <- case e of
                ExName n -> pure [(n, projectHome </> getExampleInputPath year day n)]
                ExAll -> do
                  tis <- scanForSolution projectHome (year, day)
                  pure $ fmap (\t -> (tag t, inputFilePath t)) $ sortOn tag tis
              forM_ tagAndInputFilePaths \(tag, inputFilePath) -> do
                rawContent <- System.IO.Strict.readFile inputFilePath
                putStr $ "Modifying " <> tag <> " ... "
                case consumeExtraLeadingLines rawContent of
                  (Just _, _) -> putStrLn "skipped."
                  (Nothing, _) -> do
                    writeFile inputFilePath (intercalate "\n" [exampleExtraBegin, exampleExtraEnd, rawContent])
                    putStrLn "written."
            CmdListExamples -> do
              projectHome <- getEnv "PROJECT_HOME"
              tis <- scanForSolution projectHome (year, day)
              forM_ tis \TestdataInfo {tag, inputFilePath, mExpectFilePath} -> do
                putStrLn $ tag <> ":"
                putStrLn $ "  " <> inputFilePath
                case mExpectFilePath of
                  Just ep -> putStrLn $ "  " <> ep
                  Nothing -> pure ()
            CmdDownload mDest -> do
              mySession <- getEnv "ADVENT_OF_CODE_SESSION"
              raw <- fetchInputData manager (BSC.pack mySession) year day
              case mDest of
                Just actualFp -> BSL.writeFile actualFp raw
                Nothing -> BSC.putStrLn (BSL.toStrict raw)
        Nothing ->
          die "No solution available, only `new` command is accepted."
