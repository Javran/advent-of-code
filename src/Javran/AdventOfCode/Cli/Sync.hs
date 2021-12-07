{-
  Scan src/Javran/AdventOfCode/Y<num> and update Main.hs files.
 -}

module Javran.AdventOfCode.Cli.Sync
  ( syncCommand
  , performSync
  )
where

import Control.Monad
import Data.List
import Data.Maybe
import Javran.AdventOfCode.Cli.ProgressReport as ProgressReport
import Javran.AdventOfCode.Cli.TestdataDigest
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Infra
import System.Directory
import System.Environment
import System.FilePath.Posix

import Text.ParserCombinators.ReadP
import Text.Printf

yearModuleP :: ReadP Int
yearModuleP = char 'Y' *> decimal1P

dayFileP :: ReadP Int
dayFileP = string "Day" *> decimal1P <* string ".hs"

generateModuleImports :: Int -> [Int] -> [String]
generateModuleImports yyyy dds = do
  dd <- dds
  pure $ printf "import Javran.AdventOfCode.Y%d.Day%d ()" yyyy dd

syncCommand :: SubCmdContext -> IO ()
syncCommand _ctxt = performSync

performYearlyModuleSync :: IO ()
performYearlyModuleSync = do
  projectHome <- getEnv "PROJECT_HOME"
  let baseDir = projectHome </> "src" </> "Javran" </> "AdventOfCode"
  yearDs <- listDirectory baseDir >>= filterM (\p -> doesDirectoryExist (baseDir </> p))
  forM_ yearDs $ \moduleComp -> case consumeAllWithReadP yearModuleP moduleComp of
    Just year -> do
      let yearDir = baseDir </> moduleComp
      dayFs <- listDirectory yearDir >>= filterM (\p -> doesFileExist (yearDir </> p))
      let days =
            sort $
              mapMaybe
                (consumeAllWithReadP dayFileP)
                dayFs
          moduleFp = yearDir </> "Main.hs"
      let importLines = generateModuleImports year days
          extractSecCb =
            (\prevSec bm sec em postSec ->
               if sec == importLines
                 then -- nothing is changed.
                   (error "no need for editing.", Just False)
                 else
                   ( prevSec <> [bm] <> importLines <> [em] <> postSec
                   , Just True
                   ))
      let prefix = 'Y' : show year <> ": "
      mayEditFileWithSpecialSection
        moduleFp
        prefix
        "{- ORMOLU_DISABLE -}"
        "{- ORMOLU_ENABLE -}"
        extractSecCb
    Nothing -> pure ()


performReadmeProgressSync :: IO ()
performReadmeProgressSync = do
  projectHome <- getEnv "PROJECT_HOME"
  let fp = projectHome </> "README.md"
  rendered0 <- renderRawMarkdown <$> computeProgressReport

  let rendered = "" : rendered0 <> [""]
      extractSecCb =
        (\prevSec bm sec em postSec ->
           if sec == rendered
             then -- no need for editing, nothing is changed.
               (sec, Just False)
             else
               ( prevSec <> [bm] <> rendered <> [em] <> postSec
               , Just True
               ))
  mayEditFileWithSpecialSection
    fp
    "README: "
    "[//]: # (PROGRESS_AUTOGEN_BEGIN)"
    "[//]: # (PROGRESS_AUTOGEN_END)"
    extractSecCb

performSync :: IO ()
performSync = do
  performYearlyModuleSync
  performTestdataSpecHashSync
  performReadmeProgressSync
