{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Main
  ( main
  )
where

import Control.Monad
import qualified Javran.AdventOfCode.Cli.New as CliNew
import qualified Javran.AdventOfCode.Cli.ProgressReport as CliReport
import qualified Javran.AdventOfCode.Cli.SolutionCommand as SolutionCommand
import qualified Javran.AdventOfCode.Cli.Sync as CliSync
import Javran.AdventOfCode.Infra
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Console.Terminfo
import System.Environment
import System.Exit

main :: IO ()
main = do
  t <- setupTermFromEnv
  manager <- newManager tlsManagerSettings
  let ctxt =
        SubCmdContext
          { cmdHelpPrefix = "<prog> "
          , mTerm = Just t
          , manager
          }
  getArgs >>= \case
    args
      | Just mode <- SolutionCommand.parse args ->
        SolutionCommand.subCommand ctxt mode
    subCmd : args
      | Just handler <- lookup subCmd subCmdHandlers ->
        withArgs args (handler ctxt {cmdHelpPrefix = cmdHelpPrefix ctxt <> subCmd <> " "})
    _ -> do
      forM_ subCmdHandlers $ \(sub, _) ->
        putStrLn $ cmdHelpPrefix ctxt <> sub <> " ..."
      putStrLn $  cmdHelpPrefix ctxt <> "<year> <day> ..."
      putStrLn "For listing what <year> and <day> are available, use `report` subcommand."
      exitFailure
  where
    subCmdHandlers =
      [ ("sync", CliSync.syncCommand)
      , ("new", CliNew.newCommand)
      , ("report", CliReport.progressReportCommand)
      , ("_dev", const (pure ()))
      ]
