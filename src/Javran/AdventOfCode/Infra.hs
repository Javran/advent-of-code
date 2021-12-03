{-
  This module contains intrastructure that keep things working.
 -}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Javran.AdventOfCode.Infra
  ( prepareDataPath
  , SubCmdHandlers
  , dispatchToSubCmds
  , Solution (..)
  , SolutionContext (..)
  , runSolutionWithInputGetter
  , runSolutionWithLoginInput
  , runSolutionWithExampleInput
  , runSolutionWithExampleAndWriteExpect
  , SomeSolution (..)
  , runSomeSolution
  , mkYearlyMain
  , exampleRawInputRelativePath
  , editExample
  )
where

import Control.Monad
import Control.Once
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import Data.IORef
import Data.Proxy
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Paths_advent_of_code as StockData
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.IO
import qualified Turtle.Bytes as TBytes

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

getRawInput :: Int -> Int -> IO BSL.ByteString
getRawInput yyyy dd = prepareDataPath rsc >>= BSL.readFile
  where
    rsc = show yyyy </> "day" </> show dd </> "input"

exampleRawInputRelativePath :: Int -> Int -> FilePath
exampleRawInputRelativePath yyyy dd =
  "data"
    </> "testdata"
    </> show yyyy
    </> "day"
    </> show dd

getExampleRawInput :: Int -> Int -> IO BSL.ByteString
getExampleRawInput yyyy dd = do
  projectHome <- getEnv "PROJECT_HOME"
  dataDir <- StockData.getDataDir
  let subPath = exampleRawInputRelativePath yyyy dd
      exampleInputFileName = "example.input.txt"
      fp =
        dataDir
          </> subPath
          </> exampleInputFileName
  e <- doesFileExist fp
  if e
    then BSL.readFile fp
    else do
      createDirectoryIfMissing True (projectHome </> subPath)
      hPutStrLn stderr $
        "Sample file for Solution " <> show (yyyy, dd) <> " does not exist."
      hPutStrLn stderr $
        "Please write its content to: " <> (projectHome </> subPath </> exampleInputFileName)
      exitFailure

data SolutionContext = SolutionContext
  { getInputS :: IO String
  , getInputT :: IO T.Text
  , answerS :: String -> IO ()
  , answerShow :: forall a. Show a => a -> IO ()
  , answerT :: T.Text -> IO ()
  }

class Solution sol where
  -- year and day
  solutionIndex :: forall p. p sol -> (Int, Int)
  solutionRun :: forall p. p sol -> SolutionContext -> IO ()

runSolutionWithInputGetter :: forall p sol. Solution sol => p sol -> (Int -> Int -> IO BSL.ByteString) -> IO T.Text
runSolutionWithInputGetter p inputGetter = do
  let (yyyy, dd) = solutionIndex p
  getInputBs <- once (inputGetter yyyy dd)
  outRef <- newIORef @TLB.Builder ""
  let getInputT = decodeUtf8 . BSL.toStrict <$> getInputBs
      getInputS = T.unpack <$> getInputT
      answerT output =
        atomicModifyIORef' outRef (\b -> (b <> TLB.fromText output <> "\n", ()))
      answerS output =
        atomicModifyIORef' outRef (\b -> (b <> TLB.fromString output <> "\n", ()))
      answerShow = answerS . show
  solutionRun
    p
    SolutionContext
      { getInputS
      , getInputT
      , answerT
      , answerS
      , answerShow
      }
  answer <- readIORef outRef
  pure $ TL.toStrict $ TLB.toLazyText answer

runSolutionWithExampleInput :: forall p sol. Solution sol => p sol -> IO T.Text
runSolutionWithExampleInput p = runSolutionWithInputGetter p getExampleRawInput

runSolutionWithLoginInput :: forall p sol. Solution sol => p sol -> IO T.Text
runSolutionWithLoginInput p = runSolutionWithInputGetter p getRawInput

{-
  TODO: expect files are not used for now - in future might use it as unit test.
 -}
runSolutionWithExampleAndWriteExpect :: forall p sol. Solution sol => p sol -> IO ()
runSolutionWithExampleAndWriteExpect p = do
  projectHome <- getEnv "PROJECT_HOME"
  let (yyyy, dd) = solutionIndex p
  actualOutput <- runSolutionWithExampleInput p
  let fpTarget = projectHome </> subPath </> "example.expect.txt"
      subPath = exampleRawInputRelativePath yyyy dd
  T.writeFile fpTarget actualOutput
  putStrLn $ "Written to: " <> fpTarget

data SomeSolution = forall sol. Solution sol => SomeSolution (Proxy sol)

editExample :: Int -> Int -> IO ()
editExample yyyy dd = do
  mEditorCmd <- lookupEnv "EDITOR"
  projectHome <- getEnv "PROJECT_HOME"
  let subPath = exampleRawInputRelativePath yyyy dd
      exampleInputFileName = "example.input.txt"
      exampleDir = projectHome </> subPath
  createDirectoryIfMissing True exampleDir
  let exampleFp = exampleDir </> exampleInputFileName
  -- ensure that the file exists.
  do
    e <- doesFileExist exampleFp
    unless e $ writeFile exampleFp ""
  case mEditorCmd of
    Just editorCmd | not (all isSpace editorCmd) -> do
      ec <- TBytes.proc (T.pack editorCmd) [T.pack $ exampleDir </> exampleInputFileName] ""
      exitWith ec
    _ ->
      do
        print mEditorCmd
        putStrLn "EDITOR is empty, please edit the file manually:"
        putStrLn exampleFp

runSomeSolution :: SomeSolution -> String -> IO ()
runSomeSolution (SomeSolution s) cmdHelpPrefix = do
  args <- getArgs
  case args of
    [] ->
      runSolutionWithLoginInput s >>= T.putStr
    ["login"] ->
      runSolutionWithLoginInput s >>= T.putStr
    ["example"] ->
      runSolutionWithExampleInput s >>= T.putStr
    ["edit-example"] ->
      let (yyyy, dd) = solutionIndex s
       in editExample yyyy dd
    ["write-expect"] ->
      runSolutionWithExampleAndWriteExpect s
    _ ->
      die $ cmdHelpPrefix <> "[example|login|write-expect]"

mkYearlyMain :: Int -> [SomeSolution] -> String -> IO ()
mkYearlyMain year collectedSolutions = yearlyMain
  where
    solutionRunners = mk <$> collectedSolutions
      where
        mk ss@(SomeSolution s) = case solutionIndex s of
          (yyyy, dd)
            | yyyy == year ->
              (show dd, runSomeSolution ss)
          solInd -> error $ "Invalid solution index: " <> show solInd

    yearlyMain :: String -> IO ()
    yearlyMain cmdHelpPrefix = do
      dispatchToSubCmds
        cmdHelpPrefix
        solutionRunners
