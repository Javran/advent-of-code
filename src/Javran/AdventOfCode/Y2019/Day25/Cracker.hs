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

module Javran.AdventOfCode.Y2019.Day25.Cracker
  ( runCracker
  )
where

{-
  Cracker assumes that the droid is at Security Checkpoint and
  have all items necessary for cracking and attempts to crack the keypad code.
 -}
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

{-
  TODO: this is current just a dump cracker that tries everything.
 -}
runCracker :: ExplorerResult -> IO Int
runCracker (fullInventory, psfDir, prog) = do
  let allCombinations = filterM (const [False, True]) fullInventory
      dropEverything = ["drop " <> x | x <- fullInventory]
      tryComb :: [String] -> [String]
      tryComb items =
        ["take " <> item | item <- items] <> [fmap toLower $ show psfDir]
          <> ["drop " <> item | item <- items]
      tryAllCombinations = do
        comb <- allCombinations
        tryComb comb
      presetCommands = dropEverything <> tryAllCombinations
  fix
    (\loop curP curPresetCmds -> do
       result <- curP
       case result of
         AsciiDone -> error "preset command exhausted."
         AsciiNeedCommand k -> do
           (cmd, nextPresetCmds) <- case curPresetCmds of
             [] ->
               error "preset command exhausted."
             (pCmd : cs') -> do
               pure (pCmd, cs')
           loop (k cmd) nextPresetCmds
         AsciiOutput outs k -> do
           let r = debugConsumeAllWithReadP responsesP outs
               cont = loop k curPresetCmds
           case r of
             Right
               [ RespRoomInfo
                   RoomInfo
                     { riPressureSensitiveExtra = Just (ParSuccess v)
                     }
                 ] -> pure v
             Right _ ->
               cont
             Left msg -> do
               putStrLn $ "warning: parse failure, reason: " <> msg
               putStr outs
               cont)
    prog
    presetCommands
