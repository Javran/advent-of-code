{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Javran.AdventOfCode.Prelude
  ( prepareDataPath
  , SubCmdHandlers
  , dispatchToSubCmds
  , decimal1P
  , Solution (..)
  , SolutionContext (..)
  , runSolutionWithLoginInput
  )
where

import Control.Monad
import Control.Once
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import Data.IORef
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import Text.ParserCombinators.ReadP
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

decimal1P :: (Read i, Integral i) => ReadP i
decimal1P = read <$> munch1 isDigit

getRawInput :: Int -> Int -> IO BSL.ByteString
getRawInput yyyy dd = prepareDataPath rsc >>= BSL.readFile
  where
    rsc = show yyyy </> "day" </> show dd </> "input"

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

runSolutionWithLoginInput :: forall p sol. Solution sol => p sol -> IO T.Text
runSolutionWithLoginInput p = do
  let (yyyy, dd) = solutionIndex p
  getInputBs <- once (getRawInput yyyy dd)
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
