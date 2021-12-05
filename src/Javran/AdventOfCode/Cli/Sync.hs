{-
  Scan src/Javran/AdventOfCode/Y<num> and update Main.hs files.
 -}

module Javran.AdventOfCode.Cli.Sync
  ( syncCommand
  , performSync
  , performTestdataSpecHashSync
  )
where

import Control.Monad
import Data.List
import Data.Maybe
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Tester (computeTestdataDirDigestTextRep)
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

mayEditFileWithSpecialSection
  :: FilePath
  -> String
  -> String
  -> String
  -> ExtractSectionCallback
       String
       ( [String]
       , Maybe Bool {- if this part is `Just False`, we guaranteed not to scrutinize `fst` part -}
       )
  -> IO ()
mayEditFileWithSpecialSection fp prefix bm em extractSecCb = do
  mainModuleContents <- System.IO.Strict.readFile fp
  let contentLines = lines mainModuleContents
      editResult :: ([String], Maybe Bool)
      editResult =
        extractSection
          bm
          em
          -- when the section is not found.
          (contentLines, Nothing)
          extractSecCb
          contentLines
  case editResult of
    (_, Nothing) -> do
      putStrLn $ prefix <> "Abort editing as no section is recognized."
    (_, Just False) -> do
      putStrLn $ prefix <> "No edit required."
    (xs, Just True) -> do
      writeFile fp (unlines xs)
      putStrLn $ prefix <> "Module updated."

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

performTestdataSpecHashSync :: IO ()
performTestdataSpecHashSync = do
  projectHome <- getEnv "PROJECT_HOME"
  let fp = projectHome </> "test" </> "Javran" </> "AdventOfCode" </> "TestdataSpec.hs"
  digest <- computeTestdataDirDigestTextRep
  let extractSecCb =
        (\prevSec bm sec em postSec ->
           if sec == [digest]
             then -- no need for editing, nothing is changed.
               (sec, Just False)
             else
               ( prevSec <> [bm] <> [digest] <> [em] <> postSec
               , Just True
               ))

  mayEditFileWithSpecialSection
    fp
    "Edit TestdataSpec: "
    "FORCE_RECOMP_HASH_BEGIN"
    "FORCE_RECOMP_HASH_END"
    extractSecCb

performSync :: IO ()
performSync = do
  performYearlyModuleSync
  performTestdataSpecHashSync
