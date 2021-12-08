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

import Control.Monad
import qualified Data.Text.IO as T
import Javran.AdventOfCode.Cli.EditExample
import Javran.AdventOfCode.Cli.New
import Javran.AdventOfCode.Cli.ProgressReport
import Javran.AdventOfCode.Cli.TestdataDigest
import Javran.AdventOfCode.Infra
import Javran.AdventOfCode.Solutions
import System.Console.Terminfo
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.IO

data ExampleName
  = -- | if an example name can be parsed as an unsigned int, it must.
    ExNum Word
  | -- | otherwise we need a name (must be non-empty)
    ExName String
  | -- | (only valid for edit) special name for adding examples, should resolve to an empty or non-existing example
    ExAdd
  | -- | (only valid for running) special name for running all examples
    ExAll

data Command
  = CmdRunLogin
  | CmdRunExample ExampleName
  | CmdEditExample ExampleName
  | CmdWriteExampleExpect
  | CmdNewSolution

{-
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
  performTestdataSpecHashSync
  performReadmeProgressSync

parseExampleName :: String -> Either String ExampleName
parseExampleName xs
  | null xs = Left "empty name"
  | Just v <-
      lookup
        xs
        [ ("+", ExAdd)
        , ("all", ExAll)
        ] =
    pure v
  | Just v <-
      consumeAllWithReadP @Word decimal1P xs =
    pure $ ExNum v
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

  to here with `<action> <args>...` passed as function parameter to here.
 -}
runMainWith :: SubCmdContext -> Int -> Int -> [String] -> IO ()
runMainWith SubCmdContext {cmdHelpPrefix, mTerm} year day args = do
  cmd <- case parseArgs args of
    Left msg -> do
      hPutStrLn stderr msg
      die $ cmdHelpPrefix <> "[e|example|l|login|edit-example|write-expect|new]"
    Right v -> pure v
  case cmd of
    CmdNewSolution ->
      -- new is valid whether a solution exists or not.
      newCommandForYearDay year day
    _ ->
      case getSolution year day of
        Just (SomeSolution s) ->
          case cmd of
            CmdNewSolution -> error "unreachable"
            CmdRunLogin -> void $ runSolutionWithLoginInput s True mTerm
            CmdRunExample _e -> void $ runSolutionWithExampleInput s True mTerm
            CmdEditExample _e -> editExample year day
            CmdWriteExampleExpect ->
              runSolutionWithExampleAndWriteExpect s mTerm
        Nothing ->
          die "No solution available, only `new` command is accepted."
