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

module Javran.AdventOfCode.Y2018.Day21
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Bits
import Data.Char
import qualified Data.DList as DL
import Data.Function
import Control.Monad.Writer.Lazy
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
import Debug.Trace
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2018.Day16 (BinValueMode (..), OpType (..), ValueMode (..))
import Javran.AdventOfCode.Y2018.Day19
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day21 deriving (Generic)

{-
  My login input:

  IP is bound to R3
   0    r1 = 123
   1    r1 = r1 & 456
   2    r1 = r1 == 72
  ;; for now, r1 == 72 seems to always be true.
   3    jmp r1 + 4
   4    jmp 1
   5    r1 = 0
   6    r2 = r1 | 65536
   7    r1 = 10605201
   8    r5 = r2 & 255
   9    r1 = r1 + r5
  10    r1 = r1 & 16777215
  11    r1 = r1 * 65899
  12    r1 = r1 & 16777215
  13    r5 = 256 > r2
  14    jmp r5 + 15
  15    jmp 17
  16    jmp 28
  17    r5 = 0
  18    r4 = r5 + 1
  19    r4 = r4 * 256
  20    r4 = r4 >= r2
  21    jmp r4 + 22
  22    jmp 24
  23    jmp 26
  24    r5 = r5 + 1
  25    jmp 18
  26    r2 = r5
  27    jmp 8
  ;; r0 is the only value we can change.
  28    r5 = r1 == r0
  ;; to halt, we need r1 == r0
  29    jmp r5 + 30
  30    jmp 6

 -}

interpret :: Program -> Machine -> Maybe Machine
interpret (ipReg, instrs) (ip, regsPre) =
  if ip < 0 || ip >= V.length instrs
    then Nothing
    else
      let Instr {sOp, sOperands = (a, b, c)} = instrs V.! ip
          regs = regsPre & _reg ipReg .~ ip
          getVal vm i = case vm of
            Reg ->
              regs ^. _reg (resolveReg i)
            Imm -> i
          -- category0 covers ops with prefix add / mul / ban / bor
          cat0 mb f =
            let v0 = getVal Reg a
                v1 = getVal mb b
                rOut = resolveReg c
             in regs & _reg rOut .~ f v0 v1
          -- category1 covers ops with prefix gt / eq
          cat1 mab f = do
            let (ma, mb) = case mab of
                  ImmReg -> (Imm, Reg)
                  RegImm -> (Reg, Imm)
                  RegReg -> (Reg, Reg)
                v0 = getVal ma a
                v1 = getVal mb b
                rOut = resolveReg c
             in regs & _reg rOut .~ bool 0 1 (f v0 v1)
          regs' :: (Int, Int, Int, Int, Int, Int)
          regs' = case sOp of
            Add mb -> cat0 mb (+)
            Mul mb -> cat0 mb (*)
            BitAnd mb -> cat0 mb (.&.)
            BitOr mb -> cat0 mb (.|.)
            Assign ma ->
              let v0 = getVal ma a
                  rOut = resolveReg c
               in regs & _reg rOut .~ v0
            TestGreaterThan mab -> cat1 mab (>)
            TestEqual mab -> cat1 mab (==)
          ip' :: Int
          ip' = regs' ^. _reg ipReg
       in Just (ip' + 1, regs' & _reg ipReg %~ succ)
  where
    resolveReg :: Int -> Register
    resolveReg i =
      toEnum @Register i

runProgram :: Program -> IS.IntSet -> Regs -> Writer (DL.DList Regs) Machine
runProgram prog lps inp = runAux (0, inp)
  where
    runAux :: Machine -> Writer (DL.DList Regs) Machine
    runAux m@(ip, regs) = do
      when (IS.member ip lps) do
        tell $ DL.singleton regs
      case interpret prog m of
        Nothing ->
          pure m
        Just m' ->
          runAux m'

instance Solution Day21 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    prog <- consumeOrDie programP <$> getInputS
    let result = runWriter $ runProgram prog (IS.singleton 28) (0, 0, 0, 0, 0, 0)
    pprProgram prog
    let extractInfo (_, r1, _, _, _, _) = r1
    answerShow (extractInfo $ head $ DL.toList $ snd result)
