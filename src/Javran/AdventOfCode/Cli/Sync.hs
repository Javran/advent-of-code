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
import Javran.AdventOfCode.Prelude
import System.Directory
import System.Environment
import System.FilePath.Posix
import qualified System.IO.Strict
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

syncCommand :: String -> IO ()
syncCommand _cmdHelpPrefix = performSync

performSync :: IO ()
performSync = do
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
      mainModuleContents <- System.IO.Strict.readFile moduleFp
      let importLines = generateModuleImports year days
          contentLines = lines mainModuleContents
          editResult :: ([String], Maybe Bool)
          editResult =
            extractSection
              "{- ORMOLU_DISABLE -}"
              "{- ORMOLU_ENABLE -}"
              -- when the section is not found.
              (contentLines, Nothing)
              (\prevSec bm sec em postSec ->
                 if sec == importLines
                   then -- no need for editing, nothing is changed.
                     (contentLines, Just False)
                   else
                     ( prevSec <> [bm] <> importLines <> [em] <> postSec
                     , Just True
                     ))
              contentLines
      let prefix = 'Y' : show year <> ": "
      case editResult of
        (_, Nothing) -> do
          putStrLn $ prefix <> "Abort editing as no section is recognized."
        (_, Just False) -> do
          putStrLn $ prefix <> "No edit required."
        (xs, Just True) -> do
          writeFile moduleFp (unlines xs)
          putStrLn $ prefix <> "Module updated."
    Nothing -> pure ()
