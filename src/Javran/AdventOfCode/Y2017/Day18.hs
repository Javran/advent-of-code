{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2017.Day18
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Control.Monad.Writer.CPS
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
import Data.Void
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day18 deriving (Generic)

data Reg = Reg Int deriving (Show)

type ReadVal = Either Int Reg

data Instr
  = Send ReadVal
  | Assign Reg ReadVal
  | Add Reg ReadVal
  | Mul Reg ReadVal
  | Mod Reg ReadVal
  | Recv Reg
  | JumpGtZero ReadVal ReadVal
  deriving (Show)

instrP :: ReadP Instr
instrP =
  foldl1'
    (<++)
    [ string "snd " *> (Send <$> readValP)
    , string "set " *> (Assign <$> regP <*> readValP)
    , string "add " *> (Add <$> regP <*> readValP)
    , string "mul " *> (Mul <$> regP <*> readValP)
    , string "mod " *> (Mod <$> regP <*> readValP)
    , string "rcv " *> (Recv <$> regP)
    , string "jgz " *> (JumpGtZero <$> readValP <*> readValP)
    ]
  where
    intP = readS_to_P (reads @Int)
    regP =
      (\v -> Reg $ ord v - ord 'a')
        <$> (satisfy isAsciiLower <* skipSpaces)
    readValP = (Right <$> (regP <* skipSpaces)) <++ (Left <$> intP)

registerP :: Reg
registerP = Reg $ ord 'p' - ord 'a'

{- ((pc, regs), last sent) -}
type Machine = ((Int, IM.IntMap Int), Maybe Int)

interpret :: V.Vector Instr -> Machine -> Maybe Machine
interpret instrs ((pc, regs), lastSent) = do
  when (pc < 0 || pc >= V.length instrs) do Nothing
  let getVal :: ReadVal -> Int
      getVal = \case
        Left v -> v
        Right (Reg i) -> fromMaybe 0 (regs IM.!? i)
      liftOp op rx@(Reg x) y =
        ( ( pc + 1
          , IM.insert x (op (getVal (Right rx)) (getVal y)) regs
          )
        , lastSent
        )
  pure case instrs V.! pc of
    Send x -> ((pc + 1, regs), Just (getVal x))
    Assign rx y -> liftOp (\_ y' -> y') rx y
    Add rx y -> liftOp (+) rx y
    Mul rx y -> liftOp (*) rx y
    Mod rx y -> liftOp rem rx y
    Recv rx ->
      if getVal (Right rx) == 0
        then ((pc + 1, regs), lastSent)
        else liftOp (\_ _ -> fromJust lastSent) rx (error "unused")
    JumpGtZero x y ->
      let x' = getVal x
          y' = getVal y
       in if x' > 0 then ((pc + y', regs), lastSent) else ((pc + 1, regs), lastSent)

solve :: V.Vector Instr -> Machine -> Int
solve instrs st@((pc, _), _) = case interpret instrs st of
  Nothing -> error "no recover instr"
  Just st'@(_, sent) -> case instrs V.! pc of
    Recv _ -> fromJust sent
    _ -> solve instrs st'

data ExtInt = External | Internal

type family InternalOnly ei where
  InternalOnly 'Internal = ()
  InternalOnly 'External = Void

data Result (ei :: ExtInt)
  = Done Machine2
  | Paused (InternalOnly ei) (Result ei)
  | NeedInput (Int -> Result ei)
  | SentOutput Int (Result ei)

type Machine2 = (Int, IM.IntMap Int)

tick :: V.Vector Instr -> Machine2 -> Result 'Internal
tick instrs m@(pc, regs) =
  if (pc < 0 || pc >= V.length instrs)
    then Done m
    else
      let getVal :: ReadVal -> Int
          getVal = \case
            Left v -> v
            Right (Reg i) -> fromMaybe 0 (regs IM.!? i)
          liftOp op rx@(Reg x) y =
            Paused
              ()
              (tick
                 instrs
                 ( pc + 1
                 , IM.insert x (op (getVal (Right rx)) (getVal y)) regs
                 ))
       in case instrs V.! pc of
            Send x -> SentOutput (getVal x) $ tick instrs (pc + 1, regs)
            Assign rx y -> liftOp (\_ y' -> y') rx y
            Add rx y -> liftOp (+) rx y
            Mul rx y -> liftOp (*) rx y
            Mod rx y -> liftOp rem rx y
            Recv (Reg r) -> NeedInput \inp ->
              tick instrs (pc + 1, IM.insert r inp regs)
            JumpGtZero x y ->
              let x' = getVal x
                  y' = getVal y
               in Paused () (tick instrs $ if x' > 0 then (pc + y', regs) else (pc + 1, regs))

ticks :: Result 'Internal -> Result 'External
ticks = \case
  Done m -> Done m
  Paused () k -> ticks k
  NeedInput k -> NeedInput (ticks . k)
  SentOutput o k -> SentOutput o (ticks k)

type System = ((Result 'External, Seq.Seq Int), (Result 'External, Seq.Seq Int))

runDuet :: System -> Writer (Sum Int) ()
runDuet = \case
  ((Paused v _, _), _) -> absurd v
  (_, (Paused v _, _)) -> absurd v
  ((SentOutput o r0', q0), (r1', q1)) -> runDuet ((r0', q0), (r1', q1 Seq.|> o))
  ((r0', q0), (SentOutput o r1', q1)) -> tell 1 >> runDuet ((r0', q0 Seq.|> o), (r1', q1))
  ((Done {}, _), (Done {}, _)) -> pure ()
  ((NeedInput k0, i Seq.:<| q0'), st1) -> runDuet ((k0 i, q0'), st1)
  (st0, (NeedInput k1, i Seq.:<| q1')) -> runDuet (st0, (k1 i, q1'))
  ((NeedInput {}, Seq.Empty), (Done {}, _)) -> pure ()
  ((Done {}, _), (NeedInput {}, Seq.Empty)) -> pure ()
  ((NeedInput {}, Seq.Empty), (NeedInput {}, Seq.Empty)) -> pure ()

instance Solution Day18 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    instrs <- V.fromList . fmap (consumeOrDie instrP) . lines <$> getInputS
    let initSt = ((0, IM.empty), Nothing)
    answerShow $ solve instrs initSt
    do
      let Reg p = registerP
          m0 = (ticks (tick instrs (0, IM.singleton p 0)), Seq.empty)
          m1 = (ticks (tick instrs (0, IM.singleton p 1)), Seq.empty)
          initSys :: System
          initSys = (m0, m1)
          Sum ans = execWriter (runDuet initSys)
      answerShow ans
