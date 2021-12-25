{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Javran.AdventOfCode.Y2019.Day23
  (
  )
where

import Control.Concurrent
import Control.Monad
import Control.Monad.Loops
import Control.Monad.RWS.CPS
import Data.Monoid
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2019.IntCode

data Day23 deriving (Generic)

type PacketRecv = (Int, Int)

type PacketSent = (Int, PacketRecv)

type MsgQueue = Seq.Seq PacketRecv

type StepResult =
  ( Maybe PacketSent
  , Bool -- did this computer attempt to receive but got nothing?
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
      [(Computer, Bool)]
    , ( -- the message NAT is holding
        Data.Monoid.Last PacketRecv
      , -- the Y-value of the message NAT sent to address 0 last time.
        Maybe Int
      )
    )
    IO

data Part = Part1 | Part2

stepNet :: Part -> Net (Maybe Int)
stepNet part = do
  recvs <- ask
  (computers, lastFailedRecvFlags) <- unzip <$> gets fst
  results <- liftIO $ mapM runComputer computers
  let (stepResults, computers') = unzip results
      (outgoings', failedRecvs) = unzip stepResults
      outs :: [PacketSent]
      outs = catMaybes outgoings'
      isFullNetworkIdle = and $ zipWith (&&) lastFailedRecvFlags failedRecvs
  modify (first (const (zip computers' failedRecvs)))
  unknownsPre <- forM outs \(recipient, p) -> do
    if recipient < V.length recvs
      then Nothing <$ liftIO (modifyMVar_ (recvs V.! recipient) (\q -> pure $ q Seq.|> p))
      else do
        if recipient == 255
          then
            Nothing
              <$ modify ((second . first) (<> Data.Monoid.Last (Just p)))
          else liftIO $ do
            putStrLn $ "Warning: Unknown recipient: " <> show recipient
            putStrLn $ "Packet: " <> show p
            pure (Just (recipient, p))
  let _unknowns = catMaybes unknownsPre
  yLastSent <- gets (snd . snd)
  yIsTwiceInARow <-
    if isFullNetworkIdle
      then do
        Data.Monoid.Last m <- gets (fst . snd)
        case m of
          Nothing ->
            error "Network is idle but NAT haven't received any message."
          Just p@(_x, y) -> do
            liftIO $
              modifyMVar_ (V.head recvs) (\q -> pure $ q Seq.|> p)
            modify ((second . second) (const (Just y)))
            pure (y <$ guard (Just y == yLastSent))
      else pure Nothing
  Data.Monoid.Last mToNat <- gets (fst . snd)
  let natGetsMsg :: Maybe Int
      natGetsMsg = do
        (_x, y) <- mToNat
        pure y
  pure $ case part of
    Part1 -> natGetsMsg
    Part2 -> yIsTwiceInARow

instance Solution Day23 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    code <- parseCodeOrDie <$> getInputS
    let computerCount = 50
    -- create messages queues
    (recvs :: V.Vector (MVar MsgQueue)) <-
      V.fromListN computerCount
        <$> replicateM computerCount (newMVar Seq.empty)
    let initComputers = zipWith (mkComputer code) [0 ..] (V.toList recvs)
        initSt =
          ( zip initComputers (repeat False)
          , (Data.Monoid.Last Nothing, Nothing)
          )
    (p1Ans, s1, ()) <-
      runRWST
        (untilJust (stepNet Part1))
        recvs
        initSt
    answerShow p1Ans
    {-
      Mind the ambiguity what part 2 is asking about:

      "the first Y value delivered by the NAT to the computer at address 0 twice in a row?"

      Could mean:

      1. find the first occurrence that the NAT sends message to address 0 twice in a row,
        and get its Y value as the answer.

      2. find the first reoccurrence of the same Y value in a row that NAT sends to address 0.

      Apparently part 2 was asking for 2., careless writeup.

     -}
    (p2Ans, _, ()) <-
      runRWST
        (untilJust (stepNet Part2))
        recvs
        s1
    answerShow p2Ans
