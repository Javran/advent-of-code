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
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2019.Day5
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Bool
import Data.Char
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Debug.Trace
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day5 deriving (Generic)

data ParameterMode = Position | Immediate deriving (Show)

separateOpCode :: Int -> (Int, (ParameterMode, ParameterMode, ParameterMode))
separateOpCode v = (op, (pm p1, pm p2, pm p3))
  where
    pm = \case
      0 -> Position
      1 -> Immediate
      x -> error $ "Unknown parameter mode: " <> show x

    (v0, op) = v `quotRem` 100
    (v1, p1) = v0 `quotRem` 10
    (p3, p2) = v1 `quotRem` 10

type Machine = (IM.IntMap Int, ([Int {- input -}], [Int {- output -}]))

interpret :: Int -> StateT Machine IO ()
interpret pc = do
  liftIO $ putStrLn $ "PC=" <> show pc
  mem <- gets fst
  let debug i = take i $ fmap (mem IM.!) [pc, pc + 1 ..]
      readAddr i = do
        v <- gets ((IM.!? i) . fst)
        pure $ fromMaybe 0 v
      getNum i = \case
        Position -> do
          readAddr i
        Immediate ->
          pure i
      putNum i v = \case
        Position -> do
          modify $ first (IM.insert i v)
        Immediate ->
          error "target position cannot be immediate"
  opRaw <- readAddr pc
  liftIO $ putStrLn $ "RAW OP: " <> show opRaw
  let d@(opCode, (pm1, pm2, pm3)) = separateOpCode opRaw
      performBin op = do
        aL <- readAddr (pc + 1)
        l <- getNum aL pm1
        aR <- readAddr (pc + 2)
        r <- getNum aR pm2
        dst <- readAddr (pc + 3)
        putNum dst (op l r) pm3
        interpret (pc + 4)
  liftIO $ putStrLn $ "DECODE: " <> show d
  case opCode of
    99 -> pure ()
    1 -> do
      liftIO $ print ("+", debug 4)
      performBin (+)
    2 -> do
      liftIO $ print ("*", debug 4)
      performBin (*)
    3 -> do
      liftIO $ print ("input", debug 2)
      inp <- gets (head . fst . snd)
      modify ((second . first) tail)
      dst <- readAddr (pc + 1)
      putNum dst inp pm1
      interpret (pc + 2)
    4 -> do
      liftIO $ print ("output", debug 2)
      x <- readAddr (pc+1)
      out <- getNum x pm1
      modify ((second . second) (<> [out]))
      interpret (pc + 2)
    _ -> error "Something went wrong"

instance Solution Day5 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (read @Int) . splitOn "," . head . lines <$> getInputS
    let mem = IM.fromList $ zip [0 ..] xs
    e <- execStateT (interpret 0) (mem, ([1], []))
    print e
