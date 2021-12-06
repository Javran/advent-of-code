module Javran.AdventOfCode.Main
  ( main
  )
where

import qualified Javran.AdventOfCode.Cli.New as CliNew
import qualified Javran.AdventOfCode.Cli.ProgressReport as CliReport
import qualified Javran.AdventOfCode.Cli.Sync as CliSync
import Javran.AdventOfCode.Infra
import qualified Javran.AdventOfCode.Y2020.Main as Y2020
import qualified Javran.AdventOfCode.Y2021.Main as Y2021
import System.Console.Terminfo

main :: IO ()
main = do
  t <- setupTermFromEnv
  let ctxt =
        SubCmdContext
          { cmdHelpPrefix = "<prog> "
          , mTerm = Just t
          }
  dispatchToSubCmds
    ctxt
    [ ("2020", Y2020.subMain)
    , ("2021", Y2021.subMain)
    , ("sync", CliSync.syncCommand)
    , ("new", CliNew.newCommand)
    , ("report", CliReport.progressReportCommand)
    ]
