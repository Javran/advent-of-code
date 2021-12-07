{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Javran.AdventOfCode.Cli.SolutionCommand
  ( parse
  , CommandMode (..)
  , subCommand
  )
where

{-
  This module handles all solution-specific commands,
  rather than having each year as a seperated command.

  Accepted commands:

  - "ls" for listing.
  - any integer value (<year>) commits to this subcommand.
  - <year>: list avaliable day solutions for that year
  - <year> <day> <subcommand> <subcommand args>...: this should preserve
    the old behavior.
  - <year> <day> new: create solution from template
 -}

import Control.Monad
import qualified Data.IntMap.Strict as IM
import qualified Data.Text.IO as T
import Javran.AdventOfCode.Cli.EditExample
import Javran.AdventOfCode.Cli.New
import Javran.AdventOfCode.Cli.TestdataDigest
import Javran.AdventOfCode.Infra
import Javran.AdventOfCode.Solutions
import System.Console.Terminfo
import System.Environment
import System.Exit
import System.FilePath.Posix

data CommandMode
  = ModeList [String]
  | ModeYear Int [String]
  | ModeYearDay Int Int [String]

parse :: [String] -> Maybe CommandMode
parse = \case
  "ls" : xs -> pure $ ModeList xs
  yearRaw : xs | Just year <- consumeAllWithReadP decimal1P yearRaw ->
    case xs of
      dayRaw : ys
        | Just day <- consumeAllWithReadP decimal1P dayRaw ->
          pure $ ModeYearDay year day ys
      ys -> pure $ ModeYear year ys
  _ -> Nothing

subCommand :: SubCmdContext -> CommandMode -> IO ()
subCommand ctxt mode = do
  let SubCmdContext {cmdHelpPrefix = _unused} = ctxt
  case mode of
    ModeList _ ->
      mapM_ (print . (\(SomeSolution s) -> solutionIndex s)) allSolutionsSorted
    ModeYear year _ ->
      case allSolutions IM.!? year of
        Nothing -> do
          putStrLn "No solution available, valid years:"
          print (IM.keys allSolutions)
        Just yearSols ->
          mapM_ (print . (\(_, SomeSolution s) -> solutionIndex s)) (IM.toAscList yearSols)
    ModeYearDay year day xs ->
      case getSolution year day of
        Just ss ->
          withArgs xs $ runSomeSolution ss ctxt
        Nothing ->
          case xs of
            ["new"] -> newCommandForYearDay year day
            _ ->
              die "No solution available, only `new` command is accepted."

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
