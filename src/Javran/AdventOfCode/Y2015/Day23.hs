{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Javran.AdventOfCode.Y2015.Day23
  (
  )
where

import Control.Lens
import Data.Function
import qualified Data.Vector as V
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)
import Javran.AdventOfCode.Misc (commitLeft1)

data Day23 deriving (Generic)

data Reg = RA | RB deriving (Show)

data Instr
  = Hlf Reg
  | Tpl Reg
  | Inc Reg
  | Jmp Int
  | Jie Reg Int
  | Jio Reg Int
  deriving (Show)

instrP :: ReadP Instr
instrP = nonJumps <++ jmpP <++ condJmps
  where
    regP = (RA <$ char 'a') <++ (RB <$ char 'b')
    nonJumps = do
      k <-
        commitLeft1
          [ Hlf <$ strP "hlf "
          , Tpl <$ strP "tpl "
          , Inc <$ strP "inc "
          ]
      r <- regP
      pure $ k r
    intP = do
      k <-
        (id <$ char '+')
          <++ (negate <$ char '-')
          <++ pure id
      v <- decimal1P
      pure $ k v
    jmpP = do
      strP "jmp "
      Jmp <$> intP
    condJmps = do
      k <-
        commitLeft1
          [ Jie <$ strP "jie "
          , Jio <$ strP "jio "
          ]
      r <- regP
      strP ", "
      v <- intP
      pure $ k r v

type Mem = (Int, Int)

interpret :: V.Vector Instr -> Int -> Mem -> Mem
interpret instrs = fix \go pc mem ->
  if pc < 0 || pc >= V.length instrs
    then mem
    else
      let _reg = \case
            RA -> _1'
            RB -> _2'
          liftOp f r = go (pc + 1) (mem & _reg r %~ f)
       in case instrs V.! pc of
            Hlf r -> liftOp halve r
            Tpl r -> liftOp (* 3) r
            Inc r -> liftOp (+ 1) r
            Jmp offset -> go (pc + offset) mem
            Jie r offset ->
              let v = mem ^. _reg r
               in go (if even v then pc + offset else pc + 1) mem
            Jio r offset ->
              let v = mem ^. _reg r
               in go (if v == 1 then pc + offset else pc + 1) mem

instance Solution Day23 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    instrs <- V.fromList . fmap (consumeOrDie instrP) . lines <$> getInputS
    answerShow $ snd $ interpret instrs 0 (0 :: Int, 0)
    answerShow $ snd $ interpret instrs 0 (1 :: Int, 0)
