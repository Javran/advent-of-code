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

module Javran.AdventOfCode.Y2019.Day23
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Bifunctor
import Data.Bool
import Data.Char
import Data.Either
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
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2019.IntCode
import Text.ParserCombinators.ReadP hiding (count, many)

data Day23 deriving (Generic)

type PacketRecv = (Int, Int)

type PacketSent = (Int, PacketRecv)

type MsgQueue = Seq.Seq PacketRecv

data Computer = Computer
  { cpCont :: IO (Maybe PacketSent, Computer)
  }

mkComputer :: [Int] -> Int -> MVar MsgQueue -> Computer
mkComputer code netAddr recv = Computer {cpCont}
  where
    noOp :: Computer
    noOp = Computer (pure (Nothing, noOp))

    resume :: IO (Result ()) -> IO (Maybe PacketSent, Computer)
    resume k0 = do
      result <- k0
      case result of
        Done {} -> pure (Nothing, noOp)
        NeedInput {} -> do
          mMsg <-
            modifyMVar
              recv
              (\q -> case q of
                 Seq.Empty -> pure (q, Nothing)
                 msg Seq.:<| q' -> pure (q', Just msg))
          case mMsg of
            Nothing -> do
              ([], k2) <- communicate [-1] 0 (pure result)
              pure (Nothing, Computer $ resume k2)
            Just (x, y) -> do
              ([], k2) <- communicate [x, y] 0 (pure result)
              pure (Nothing, Computer $ resume k2)
        SentOutput {} -> do
          ([recipient, x, y], k1) <- communicate [] 3 (pure result)
          pure (Just (recipient, (x, y)), Computer $ resume k1)

    cpCont :: IO (Maybe PacketSent, Computer)
    cpCont = do
      ([], prog) <- communicate [netAddr] 0 (startProgramFromFoldable code)
      pure (Nothing, Computer $ resume (void <$> prog))

stepNetwork :: V.Vector (MVar MsgQueue) -> [Computer] -> IO (Maybe Int, [Computer])
stepNetwork recvs computers = do
  results <- forM computers \(Computer c) -> c
  let (outgoings', computers') = unzip results
      outs :: [PacketSent]
      outs = catMaybes outgoings'
  unknownsPre <- forM outs \(recipient, p) -> do
    if recipient < V.length recvs
      then Nothing <$ modifyMVar_ (recvs V.! recipient) (\q -> pure $ q Seq.|> p)
      else do
        putStrLn $ "Unknown recipient: " <> show recipient
        putStrLn $ "Packet: " <> show p
        pure (Just (recipient, p))
  let unknowns = catMaybes unknownsPre
  pure
    ( case unknowns of
        (_r, (_x, y)) : _ -> Just y
        [] -> Nothing
    , computers'
    )

instance Solution Day23 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    code <- parseCodeOrDie <$> getInputS
    let computerCount = 50
    -- create messages queues
    (recvs :: V.Vector (MVar MsgQueue)) <-
      V.fromListN computerCount
        <$> replicateM computerCount (newMVar (Seq.empty))
    let initComputers = zipWith (mkComputer code) [0 ..] (V.toList recvs)
        stepper = stepNetwork recvs

    fix
      (\loop computers -> do
         (m, computers') <- stepper computers
         case m of
           Nothing -> loop computers'
           Just v -> answerShow v)
      initComputers
