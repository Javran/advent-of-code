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
import Control.Monad.RWS.CPS
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

type StepResult =
  ( Maybe PacketSent
  , Bool -- should we consider this computer idle?
  )

newtype Computer = Computer
  { runComputer :: IO (StepResult, Computer)
  }

mkComputer :: [Int] -> Int -> MVar MsgQueue -> Computer
mkComputer code netAddr recv = Computer cpInit
  where
    noOp :: Computer
    noOp = Computer (pure ((Nothing, True), noOp))

    resume :: IO (Result ()) -> IO (StepResult, Computer)
    resume k0 = do
      result <- k0
      case result of
        Done {} -> pure ((Nothing, True), noOp)
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
              pure ((Nothing, True), Computer $ resume k2)
            Just (x, y) -> do
              ([], k2) <- communicate [x, y] 0 (pure result)
              pure ((Nothing, False), Computer $ resume k2)
        SentOutput {} -> do
          ([recipient, x, y], k1) <- communicate [] 3 (pure result)
          pure ((Just (recipient, (x, y)), False), Computer $ resume k1)

    cpInit :: IO (StepResult, Computer)
    cpInit = do
      ([], prog) <- communicate [netAddr] 0 (startProgramFromFoldable code)
      pure ((Nothing, False), Computer $ resume (void <$> prog))

type Net =
  RWST
    -- msgqueue references
    (V.Vector (MVar MsgQueue))
    ()
    ( -- computer states
      [Computer]
    , ( -- the message NAT is holding
        Data.Monoid.Last PacketRecv
      , -- whether the network is idle in last step
        Bool
      )
    )
    IO

stepNet :: Net (Maybe Int)
stepNet = do
  recvs <- ask
  computers <- gets fst
  results <- liftIO $ mapM runComputer computers
  let (stepResults, computers') = unzip results
      (outgoings', idleFlags) = unzip stepResults
      isFullNetworkIdle = and idleFlags
      outs :: [PacketSent]
      outs = catMaybes outgoings'
  modify (first (const computers'))
  unknownsPre <- forM outs \(recipient, p) -> do
    if recipient < V.length recvs
      then Nothing <$ (liftIO $ modifyMVar_ (recvs V.! recipient) (\q -> pure $ q Seq.|> p))
      else do
        if recipient == 255
          then Nothing <$ modify ((second . first) (<> Data.Monoid.Last (Just p)))
          else liftIO $ do
            putStrLn $ "Warning: Unknown recipient: " <> show recipient
            putStrLn $ "Packet: " <> show p
            pure (Just (recipient, p))
  let _unknowns = catMaybes unknownsPre
  _isFullNetworkIdle <- gets (snd . snd)
  when isFullNetworkIdle do
    Data.Monoid.Last m <- gets (fst . snd)
    case m of
      Nothing ->
        pure ()
      -- error "Network idle but NAT haven't received any message."
      Just p ->
        liftIO $ modifyMVar_ (V.head recvs) (\q -> pure $ q Seq.|> p)
    modify ((second . second) (const isFullNetworkIdle))

  Data.Monoid.Last mToNat <- gets (fst . snd)
  pure $ do
    (_x, y) <- mToNat
    pure y

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
    (a, _s, ()) <-
      runRWST
        (fix \cont -> do
           result <- stepNet
           case result of
             Nothing -> cont
             Just v -> pure v)
        recvs
        (initComputers, (Data.Monoid.Last Nothing, False))
    answerShow a
