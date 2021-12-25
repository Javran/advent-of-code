{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2019.Day7
  (
  )
where

import Control.Monad
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Vector.Unboxed as VU
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2019.IntCode

data Day7 deriving (Generic)

pipeProgram
  :: IO (Result (VU.Vector Int))
  -> IO (Result (VU.Vector Int))
  -> IO (Result (VU.Vector Int))
pipeProgram lProg rProg = do
  r <- rProg
  case r of
    Done v -> pure (Done v)
    NeedInput rK -> do
      l <- lProg
      case l of
        Done _ -> error "input exhausted"
        NeedInput lK ->
          pure $
            NeedInput $ \input ->
              pipeProgram (lK input) (pure (NeedInput rK))
        SentOutput lOut lK ->
          pipeProgram lK (rK lOut)
    SentOutput o rK -> pure $ SentOutput o (pipeProgram lProg rK)

instance Solution Day7 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (extraOps, rawInput) <- consumeExtraLeadingLines <$> getInputS
    let xs = fmap (read @Int) . splitOn "," . head . lines $ rawInput
        mem = VU.fromList xs
        runPart1 = maybe True ("part1" `elem`) extraOps
        runPart2 = maybe True ("part2" `elem`) extraOps
    when
      runPart1
      do
        signals <- forM (permutations [0 .. 4]) $ \config -> do
          progs <- forM config $ \c -> do
            r <- startProgram mem
            case r of
              NeedInput k ->
                pure $ k c
              _ -> error "unexpected"
          let pipeline = foldl1 pipeProgram progs
          r0 <- pipeline
          case r0 of
            NeedInput k -> do
              r1 <- k 0
              case r1 of
                SentOutput o _ -> pure o
                _ -> error "unexpected"
            _ -> error "unexpected"
        answerShow $ maximum signals
    when
      runPart2
      do
        signals <- forM (permutations [5 .. 9]) $ \config -> do
          progs <- forM config $ \c -> do
            r <- startProgram mem
            case r of
              NeedInput k ->
                pure $ k c
              _ -> error "unexpected"
          let pipeline = foldl1 pipeProgram progs
          fromJust
            <$> fix
              (\loopback curProg nextInput lastOut -> do
                 r0 <- curProg
                 case r0 of
                   NeedInput k0 ->
                     loopback (k0 $ fromJust nextInput) Nothing lastOut
                   SentOutput o k1 ->
                     loopback k1 (Just o) (Just o)
                   Done {} -> pure lastOut)
              pipeline
              (Just 0)
              Nothing
        answerShow $ maximum signals
