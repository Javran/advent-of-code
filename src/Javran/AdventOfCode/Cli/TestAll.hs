{-# LANGUAGE OverloadedStrings #-}

module Javran.AdventOfCode.Cli.TestAll
  ( testAllCommand
  )
where

import Filesystem.Path.CurrentOS
import Javran.AdventOfCode.Infra
import System.Environment
import System.Exit
import Turtle.Prelude

testAllCommand :: SubCmdContext -> IO ()
testAllCommand _ = do
  projectHome <- getEnv "PROJECT_HOME"
  cd (decodeString projectHome)
  ec <- proc "stack" ["test", "advent-of-code:test:hspec"] ""
  exitWith ec
