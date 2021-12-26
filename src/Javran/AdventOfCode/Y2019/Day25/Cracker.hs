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

type Cracker = StateT CrackerState IO

data CrackerState = CrackerState
  { csSearchSpace :: S.Set (S.Set String) -- current search space
  , csInventory :: S.Set String -- current inventory
  , csProg :: IO AsciiResult
  }

takeOrDropItem :: Bool -> String -> Cracker ()
takeOrDropItem isTake item = do
  p <- gets csProg
  r0 <- liftIO p
  case r0 of
    AsciiNeedCommand k -> do
      AsciiOutput _ k' <- liftIO $ k $ (if isTake then "take " else "drop ") <> item
      modify \cs -> cs {csProg = k'}
    _ -> error "unexpected"

crack :: Dir -> Cracker Int
crack psfDir = do
  ss <- gets csSearchSpace
  -- liftIO $ putStrLn $ "Search space size: " <> show (S.size ss)
  when (null ss) $
    error "search space exhausted"
  let tryInv = S.toList ss !! (S.size ss `quot` 2)
  -- TODO: this can be smarter
  curInv <- gets csInventory
  forM_ (S.toList curInv) \item ->
    takeOrDropItem False item
  forM_ (S.toList tryInv) \item ->
    takeOrDropItem True item
  modify \cs -> cs {csInventory = tryInv}
  p0 <- gets csProg
  AsciiNeedCommand k0 <- liftIO p0
  AsciiOutput out k1 <- liftIO $ k0 (fmap toLower $ show psfDir)
  modify \cs -> cs {csProg = k1}
  case consumeAllWithReadP responsesP out of
    Just
      [ RespRoomInfo
          RoomInfo
            { riPressureSensitiveExtra =
              Just (ParFailure {parFailureShouldBeLighter})
            }
        , _
        ] -> do
        {-
          when parFailureShouldBeLighter is True:
            the expected droid weight is lighter than we are,
            meaning everything containing current inventory cannot be the solution.
         -}
        let shouldExclude =
              if parFailureShouldBeLighter
                then (tryInv `S.isSubsetOf`)
                else (`S.isSubsetOf` tryInv)
        modify \cs -> cs {csSearchSpace = S.filter (not . shouldExclude) $ csSearchSpace cs}
        crack psfDir
    Just
      [ RespRoomInfo
          RoomInfo
            { riPressureSensitiveExtra =
              Just (ParSuccess ans)
            }
        ] -> pure ans
    xs -> error $ "unexpected response: " <> show xs

runCracker :: ExplorerResult -> IO Int
runCracker (fullInventory, psfDir, prog) = do
  let fullInv = S.fromList fullInventory
  evalStateT
    (crack psfDir)
    CrackerState
      { csSearchSpace = S.powerSet fullInv
      , csInventory = fullInv
      , csProg = prog
      }
