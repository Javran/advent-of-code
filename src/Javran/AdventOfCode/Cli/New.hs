{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Javran.AdventOfCode.Cli.New
  ( newCommand
  , newCommandForYearDay
  )
where

{-
  Command for create a new solution from template and
  do other appropriate setups.
 -}

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Lazy.IO as TL
import Javran.AdventOfCode.Cli.Sync (performSync)
import Javran.AdventOfCode.Cli.EditExample
import Javran.AdventOfCode.Infra
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import Text.Microstache

{-
  Create solution from template.

  Note that this action should be idempotent and never modify existing files.
 -}
newCommandForYearDay :: Int -> Int -> IO ()
newCommandForYearDay year day = do
  projectHome <- getEnv "PROJECT_HOME"
  let yearDirFp =
        projectHome
          </> "src"
          </> "Javran"
          </> "AdventOfCode"
          </> ('Y' : show year)

      moduleFp = yearDirFp </> ("Day" <> show day <> ".hs")
  createDirectoryIfMissing True yearDirFp
  -- module file creation.
  do
    e <- doesFileExist moduleFp
    if e
      then do
        putStrLn $ "File already exists: " <> moduleFp
        putStrLn "Skipped module file creation."
      else do
        let tmplFp = projectHome </> "data" </> "DayX.hs.mustache"
            ctxt =
              Object $
                HM.fromList
                  [ ("year", Number $ fromIntegral year)
                  , ("day", Number $ fromIntegral day)
                  ]

        tmpl <- compileMustacheFile tmplFp
        TL.writeFile moduleFp (renderMustache tmpl ctxt)
        putStrLn $ "Written to: " <> moduleFp
  -- sync modules. editor could fail or block so we do that earlier.
  performSync
  editExample year day

newCommand :: SubCmdContext -> IO ()
newCommand SubCmdContext {cmdHelpPrefix} =
  getArgs >>= \case
    [yearRaw, dayRaw]
      | [(year, "")] <- reads yearRaw
        , [(day, "")] <- reads dayRaw ->
        newCommandForYearDay year day
    _ -> die $ cmdHelpPrefix <> "<year> <day>"
