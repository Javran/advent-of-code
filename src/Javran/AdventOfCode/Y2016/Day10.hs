{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2016.Day10
  (
  )
where

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Writer.CPS hiding (First)
import qualified Data.IntMap.Strict as IM
import qualified Data.PSQueue as PQ
import Data.Semigroup
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.TestExtra
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day10 deriving (Generic)

type Instr = Either (Int, Int) (Int, (Target, Target))

data Target
  = TBot Int
  | TOut Int
  deriving (Show)

instrP :: ReadP Instr
instrP = goesToP <++ givesP
  where
    targetP =
      (string "bot " *> (TBot <$> decimal1P))
        <++ (string "output " *> (TOut <$> decimal1P))
    goesToP =
      Left <$> do
        _ <- string "value "
        v <- decimal1P
        _ <- string " goes to bot "
        t <- decimal1P
        pure (t, v)
    givesP :: ReadP Instr
    givesP =
      Right <$> do
        _ <- string "bot "
        b <- decimal1P
        _ <- string " gives low to "
        tLo <- targetP
        _ <- string " and high to "
        tHi <- targetP
        pure (b, (tLo, tHi))

type BotHolding = IM.IntMap [Int]

type GiveRules = IM.IntMap (Target, Target)

type OutBin = IM.IntMap [Int]

parseFromRaw :: [String] -> (BotHolding, GiveRules)
parseFromRaw xs =
  ( IM.fromListWith (<>) $ (fmap . second) (: []) ls
  , IM.fromListWith (error "rule conflict") rs
  )
  where
    (ls, rs) = partitionEithers $ fmap (consumeOrDie instrP) xs

type Sim = WriterT (Maybe (First Int)) (State (BotHolding, OutBin))

doneSim :: Sim Int
doneSim = do
  let exactlyOne ~[x] = x
  ~[a, b, c] <- mapM (\i -> gets (exactlyOne . (IM.! i) . snd)) [0, 1, 2]
  pure $ a * b * c

simulate :: GiveRules -> MinMax Int -> PQ.PSQ Int (Down Int) -> Sim Int
simulate rs tracedPair q0 = case PQ.minView q0 of
  Nothing -> doneSim
  Just (bot PQ.:-> Down cnt, q1) ->
    if
        | cnt == 2 -> do
          let (loTo, hiTo) = rs IM.! bot
          curMinMax@(MinMax (lo, hi)) <- do
            ~[a, b] <- gets ((IM.! bot) . fst)
            pure $ minMaxFromPair (a, b)
          modify (first (IM.delete bot))
          let process chip = \case
                TOut o -> do
                  modify (second (IM.insertWith (<>) o [chip]))
                  pure Nothing
                TBot b -> do
                  modify (first (IM.insertWith (<>) b [chip]))
                  l <- gets (length . (IM.! b) . fst)
                  pure ((b, l) <$ guard (l >= 2))
          m0 <- process lo loTo
          m1 <- process hi hiTo
          let q2 = foldr (\(b, l) -> PQ.insert b (Down l)) q1 (catMaybes [m0, m1])
          when (curMinMax == tracedPair) $
            tell (Just (First bot))
          simulate rs tracedPair q2
        | cnt > 2 -> error "bot holding more than 2 microchips"
        | cnt <= 1 -> doneSim
        | otherwise -> unreachable

instance Solution Day10 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (extraOps, rawInput) <- consumeExtra getInputS
    let (bh, gr) = parseFromRaw . lines $ rawInput
        initSt = (bh, IM.empty)
        initQ = PQ.fromList do
          (b, hs) <- IM.toList bh
          guard $ length hs >= 2
          pure (b PQ.:-> Down (length hs))
        tracedPair = minMaxFromPair $ singleLineExtra (17, 61) extraOps
        (ans2, Just (First ans1)) =
          evalState (runWriterT (simulate gr tracedPair initQ)) initSt
    answerShow ans1
    answerShow ans2
