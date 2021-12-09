{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Javran.AdventOfCode.Cli.EditExample
  ( editExample
  , editExampleWithName
  )
where

import Control.Monad
import Data.Char
import qualified Data.Text as T
import Javran.AdventOfCode.Cli.TestdataDigest
import Javran.AdventOfCode.Infra
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import qualified Turtle.Bytes as TBytes

editExampleWithName :: Int -> Int -> FilePath -> IO ()
editExampleWithName yyyy dd eName = do
  mEditorCmd <- lookupEnv "EDITOR"
  projectHome <- getEnv "PROJECT_HOME"
  let subPath = exampleRawInputRelativePath yyyy dd
      exampleInputFileName = eName <> ".input.txt"
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
      when
        (ec == ExitSuccess)
        performTestdataSpecHashSync
      exitWith ec
    _ ->
      do
        print mEditorCmd
        putStrLn "EDITOR is empty, please edit the file manually:"
        putStrLn exampleFp

editExample :: Int -> Int -> IO ()
editExample yyyy dd = editExampleWithName yyyy dd "example"
