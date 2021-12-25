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
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2019.IntCode
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day25 deriving (Generic)

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

instance Solution Day25 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    code <- parseCodeOrDie <$> getInputS
    let prog = startProgramFromFoldable code
    fix
      (\loop curP curPresetCmds -> do
         result <- curP
         case result of
           Done _ -> pure ()
           NeedInput {} -> do
             (cmd, nextPresetCmds) <- case curPresetCmds of
               [] -> do
                 xs <- getLine
                 pure (xs, [])
               (pCmd : cs') -> do
                 putStrLn $ "Enter preset command: " <> pCmd
                 pure (pCmd, cs')
             ([], k) <- communicate (fmap ord cmd <> [10]) 0 (pure result)
             loop k nextPresetCmds
           SentOutput v k -> do
             if v <= 255
               then putStr [chr v]
               else putStrLn $ "output value: " <> show v
             loop k curPresetCmds)
      prog
      presetCommands
