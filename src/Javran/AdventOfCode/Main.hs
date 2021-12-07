{-# LANGUAGE LambdaCase #-}

module Javran.AdventOfCode.Main
  ( main
  )
where

import qualified Javran.AdventOfCode.Cli.New as CliNew
import qualified Javran.AdventOfCode.Cli.ProgressReport as CliReport
import qualified Javran.AdventOfCode.Cli.SolutionCommand as SolutionCommand
import qualified Javran.AdventOfCode.Cli.Sync as CliSync
import Javran.AdventOfCode.Infra
import System.Console.Terminfo
import System.Environment

main :: IO ()
main = do
  t <- setupTermFromEnv
  let ctxt =
        SubCmdContext
          { cmdHelpPrefix = "<prog> "
          , mTerm = Just t
          }
  getArgs >>= \case
    args
      | Just mode <- SolutionCommand.parse args ->
        SolutionCommand.subCommand ctxt mode
    _ -> do
      -- TODO: solution help not discoverable from here.
      dispatchToSubCmds
        ctxt
        [ ("sync", CliSync.syncCommand)
        , ("new", CliNew.newCommand)
        , ("report", CliReport.progressReportCommand)
        ]

{-
  TODO: can we submit from cli?

  form data:
  - level=1&answer=xxxx
  - level=2&answer=xxxx

  request url: https://adventofcode.com/<year>/day/<day>/answer

  method POST

 -}
