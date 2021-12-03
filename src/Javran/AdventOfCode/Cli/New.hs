{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Javran.AdventOfCode.Cli.New
  ( newCommand
  )
where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Lazy.IO as TL
import Javran.AdventOfCode.Cli.Sync (performSync)
import Javran.AdventOfCode.Infra
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import Text.Microstache

{-
  TODO:

  - create module file by applying the template.
  - perform sync.
  - create an empty testdata input file.

 -}
newCommand :: String -> IO ()
newCommand cmdHelpPrefix =
  getArgs >>= \case
    [yearRaw, dayRaw]
      | [(year, "")] <- reads yearRaw
        , [(day, "")] <- reads dayRaw -> do
        projectHome <- getEnv "PROJECT_HOME"
        let moduleFp =
              projectHome
                </> "src"
                </> "Javran"
                </> "AdventOfCode"
                </> ('Y' : show year)
                </> ("Day" <> show day <> ".hs")
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
        -- sync modules
        performSync
        editExample year day
    _ -> die $ cmdHelpPrefix <> "<year> <day>"
