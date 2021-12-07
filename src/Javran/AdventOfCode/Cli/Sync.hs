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
import Javran.AdventOfCode.Cli.ProgressReport
import Javran.AdventOfCode.Cli.TestdataDigest
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

performSolutionsModuleSync :: IO ()
performSolutionsModuleSync = do
  projectHome <- getEnv "PROJECT_HOME"
  let baseDir = projectHome </> "src" </> "Javran" </> "AdventOfCode"
  yearDs <- listDirectory baseDir >>= filterM (\p -> doesDirectoryExist (baseDir </> p))
  -- collect import lines.
  importLines <- fmap concat <$> forM (sort yearDs) $ \moduleComp -> case consumeAllWithReadP yearModuleP moduleComp of
    Just year -> do
      let yearDir = baseDir </> moduleComp
      dayFs <- listDirectory yearDir >>= filterM (\p -> doesFileExist (yearDir </> p))
      let days = sort $ mapMaybe (consumeAllWithReadP dayFileP) dayFs
      pure $ generateModuleImports year days
    Nothing -> pure []
  -- write to the Solutions module.
  do
    let moduleFp = projectHome </> "src" </> "Javran" </> "AdventOfCode" </> "Solutions.hs"
        extractSecCb =
          (\prevSec bm sec em postSec ->
             if sec == importLines
               then -- nothing is changed.
                 (error "no need for editing.", Just False)
               else
                 ( prevSec <> [bm] <> importLines <> [em] <> postSec
                 , Just True
                 ))

    mayEditFileWithSpecialSection
      moduleFp
      "Solutions.hs: "
      "{- ORMOLU_DISABLE -}"
      "{- ORMOLU_ENABLE -}"
      extractSecCb

performSync :: IO ()
performSync = do
  performSolutionsModuleSync
  performTestdataSpecHashSync
  performReadmeProgressSync
