{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2019.Day25
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Control.Monad.Combinators
import Control.Monad.State.Strict
import Data.Char
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Debug.Trace (traceShow)
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2019.Day25.Common
import Javran.AdventOfCode.Y2019.Day25.Explorer
import Javran.AdventOfCode.Y2019.Day25.ResponseParser
import Javran.AdventOfCode.Y2019.IntCode
import Text.ParserCombinators.ReadP hiding (count, get, many, manyTill)

data Day25 deriving (Generic)

allItems :: [String]
allItems =
  [ "cake"
  , "fuel cell"
  , "easter egg"
  , "ornament"
  , "hologram"
  , "dark matter"
  , "klein bottle"
  , "hypercube"
  ]

presetCommands :: [String]
presetCommands =
  [ "north"
  , "north"
  , "east"
  , "east" -- at Crew Quarters
  , "take cake"
  , "west"
  , "west"
  , "south"
  , "south" -- at Hull Breach
  , "south"
  , "west" -- at Hallway
  , "take fuel cell"
  , "west" -- at Warp Drive Maintenance
  , "take easter egg"
  , "east"
  , "east"
  , "north" -- at Hull Breach
  , "east"
  , "take ornament"
  , "east"
  , "take hologram"
  , "east"
  , "take dark matter"
  , "north"
  , "north"
  , "east"
  , "take klein bottle"
  , "north"
  , "take hypercube"
  , "north" -- at Security Checkpoint
  ]
    <> dropEverything
    <> tryAllCombinations
  where
    dropEverything = ["drop " <> x | x <- allItems]

    allCombinations = filterM (const [False, True]) allItems

    tryComb :: [String] -> [String]
    tryComb items =
      ["take " <> item | item <- items] <> ["west"]
        <> ["drop " <> item | item <- items]
    tryAllCombinations = do
      comb <- allCombinations
      tryComb comb

{-
  TODO:

  the sequence of preset command only works on my specific input,
  we need few parts to be more flexible to solve this puzzle in general:

  - bruteforce or do something smarter.

 -}
instance Solution Day25 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    code <- parseCodeOrDie <$> getInputS
    let prog = asciiRun $ startProgramFromFoldable code
        shouldRunProgram = False
    unless shouldRunProgram do
      (inventory, psfDir, prog') <- runExplorer prog
      print (inventory, psfDir)
    when shouldRunProgram do
      fix
        (\loop curP curPresetCmds -> do
           result <- curP
           case result of
             AsciiDone -> pure ()
             AsciiNeedCommand k -> do
               (cmd, nextPresetCmds) <- case curPresetCmds of
                 [] -> do
                   xs <- getLine
                   pure (xs, [])
                 (pCmd : cs') -> do
                   putStrLn $ "Enter preset command: " <> pCmd
                   pure (pCmd, cs')
               loop (k cmd) nextPresetCmds
             AsciiOutput outs k -> do
               let r = debugConsumeAllWithReadP responsesP outs
               case r of
                 Right v -> print v
                 Left msg -> do
                   putStrLn $ "warning: parse failure, reason: " <> msg
                   putStr outs
               loop k curPresetCmds)
        prog
        presetCommands
