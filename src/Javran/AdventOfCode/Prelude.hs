{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Javran.AdventOfCode.Prelude
  ( PreparableData (..)
  , prepareDataPath
  , getInput
  , SubCmdHandlers
  , dispatchToSubCmds
  , decimal1P
  )
where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import Text.ParserCombinators.ReadP
import qualified Turtle.Bytes as TBytes

class PreparableData a where
  prepareData :: FilePath -> IO a

instance PreparableData String where
  prepareData = readFile

instance PreparableData T.Text where
  prepareData = T.readFile

instance PreparableData TL.Text where
  prepareData = TL.readFile

instance PreparableData BS.ByteString where
  prepareData = BS.readFile

instance PreparableData BSL.ByteString where
  prepareData = BSL.readFile

{-
  Ensure that the resource is available locally.
 -}
prepareDataPath :: FilePath -> IO FilePath
prepareDataPath rsc = do
  projectHome <- getEnv "PROJECT_HOME"
  mySession <- getEnv "ADVENT_OF_CODE_SESSION"

  let actualFp = projectHome </> "data" </> "download" </> rsc
      (actualDir, _) = splitFileName actualFp
  createDirectoryIfMissing True actualDir
  e <- doesFileExist actualFp
  actualFp
    <$ unless
      e
      (do
         -- there are too much bullshit involved to get the fucking CookieJar attached to a request for http-client that I won't bother.
         let url = "https://adventofcode.com" </> rsc
         (ExitSuccess, raw) <-
           TBytes.procStrict
             "curl"
             ["--cookie", "session=" <> T.pack mySession, T.pack url]
             ""
         BS.writeFile actualFp raw)

getInput :: PreparableData d => Int -> Int -> IO d
getInput yyyy dd = prepareDataPath rsc >>= prepareData
  where
    rsc = show yyyy </> "day" </> show dd </> "input"

{-
  TODO: we probably want to pass down a context rather than String, which should allow
  passing a reading IO action that can read input from somewhere else - we can allow easier testing this way.
 -}
type SubCmdHandlers = [(String, String -> IO ())]

dispatchToSubCmds :: String -> SubCmdHandlers -> IO ()
dispatchToSubCmds cmdHelpPrefix subCmdHandlers =
  getArgs >>= \case
    subCmd : args
      | Just handler <- lookup subCmd subCmdHandlers ->
        withArgs args (handler (cmdHelpPrefix <> subCmd <> " "))
    _ -> do
      forM_ subCmdHandlers $ \(sub, _) ->
        putStrLn $ cmdHelpPrefix <> sub <> " ..."
      exitFailure

decimal1P :: (Read i, Integral i) => ReadP i
decimal1P = read <$> munch1 isDigit
