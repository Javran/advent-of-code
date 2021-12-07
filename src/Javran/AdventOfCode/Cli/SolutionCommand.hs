{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Cli.SolutionCommand
  (
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

import Javran.AdventOfCode.Infra
import System.Environment

subCommand :: SubCmdContext -> Int -> IO ()
subCommand ctxt year = do
  let SubCmdContext {cmdHelpPrefix} = ctxt
  getArgs >>= \case
    args -> mapM_ print args
