{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Cli.SolutionCommand
  ( isSolutionCommand
  , CommandMode (..)
  , subCommand
  )
where

{-
  TODO: this will handle all solution-specific commands in the future,
  rather than having each year as a seperated command.

  Plan:

  - import update automation for Javran.AdventOfCode.Solutions
  - parse command line args in this module, dispatch to specific solutions or actions.

  Parsing:

  - any integer value (<year>) commits to this subcommand.

  - <year>: list avaliable day solutions for that year
  - <year> <day> <subcommand> <subcommand args>...: this should preserve
    the old behavior.

 -}

import Control.Monad
import Javran.AdventOfCode.Infra
import Javran.AdventOfCode.Solutions
import System.Environment

data CommandMode
  = ModeList [String]
  | ModeYear Int [String]
  | ModerYearDay Int Int [String]

isSolutionCommand :: [String] -> Maybe CommandMode
isSolutionCommand = \case
  "ls" : xs -> pure $ ModeList xs
  yearRaw : xs | Just year <- consumeAllWithReadP decimal1P yearRaw ->
    case xs of
      dayRaw : ys
        | Just day <- consumeAllWithReadP decimal1P dayRaw ->
          pure $ ModerYearDay year day ys
      ys -> pure $ ModeYear year ys
  _ -> Nothing

subCommand :: SubCmdContext -> CommandMode -> IO ()
subCommand ctxt mode = do
  let SubCmdContext {cmdHelpPrefix} = ctxt
  case mode of
    ModeList _ -> mapM_ (print . (\(SomeSolution s) -> solutionIndex s)) allSolutionsSorted
    _ -> error "todo"
