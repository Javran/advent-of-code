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
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2018.Day19
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Bits
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
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2018.Day16 (BinValueMode (..), OpType (..), ValueMode (..))
import Text.ParserCombinators.ReadP hiding (count, get, many)
import Text.Printf
import Debug.Trace
import Math.NumberTheory.Primes

data Day19 deriving (Generic)

instrP :: ReadP Instr
instrP = do
  opName <- munch1 isAlpha
  Just sOp <- pure (ops M.!? opName)
  ~[a, b, c] <- replicateM 3 (char ' ' *> readS_to_P (reads @Int))
  pure Instr {sOp, sOperands = (a, b, c)}

data Register = R0 | R1 | R2 | R3 | R4 | R5 deriving (Enum, Show)

type Program =
  ( Register -- register that binds to ip.
  , V.Vector Instr
  )

programP :: ReadP Program
programP = do
  let nl = void (char '\n')
  p <- string "#ip " *> decimal1P
  nl
  guard $ 0 <= p && p <= 5
  xs <- many (instrP <* nl)
  pure (toEnum p, V.fromList xs)

data Instr = Instr
  { sOp :: OpType
  , sOperands :: (Int, Int, Int)
  }
  deriving (Show)

pprInstr :: Register -> Instr -> String
pprInstr ipReg Instr {sOp, sOperands = (a, b, c)} = case sOp of
  Add m -> ppr0 "+" m
  Mul m -> ppr0 "*" m
  BitAnd m -> ppr0 "&" m
  BitOr m -> ppr0 "|" m
  Assign m -> printf "%s = %s" (mode c Reg) (mode a m)
  TestEqual ms -> ppr1 "==" ms
  TestGreaterThan ms -> ppr1 ">=" ms
  where
    mode x = \case
      Reg -> if x == fromEnum ipReg then "ip" else "r" <> show x
      Imm -> show x
    ppr0 opStr m =
      printf
        "%s = %s %s %s"
        (mode c Reg)
        (mode a Reg)
        opStr
        (mode b m)
    ppr1 opStr ms =
      printf
        "%s = %s %s %s"
        (mode c Reg)
        (mode a m1)
        opStr
        (mode b m2)
      where
        (m1, m2) = case ms of
          ImmReg -> (Imm, Reg)
          RegImm -> (Reg, Imm)
          RegReg -> (Reg, Reg)

ops :: M.Map String OpType
ops =
  M.fromList $
    (do
       (name, constr) <-
         [ ("add", Add)
           , ("mul", Mul)
           , ("ban", BitAnd)
           , ("bor", BitOr)
           , ("set", Assign)
           ]
       (suf, m) <- [('r', Reg), ('i', Imm)]
       pure (name <> [suf], constr m))
      <> (do
            (name, constr) <- [("gt", TestGreaterThan), ("eq", TestEqual)]
            (suf, m) <- [("ir", ImmReg), ("ri", RegImm), ("rr", RegReg)]
            pure (name <> suf, constr m))

type Machine = (Int, (Int, Int, Int, Int, Int, Int))

interpret :: Program -> Machine -> Maybe Machine
interpret (ipReg, instrs) (ip, regsPre) =
  if ip < 0 || ip >= V.length instrs
    then Nothing
    else
      let Instr {sOp, sOperands = (a, b, c)} = instrs V.! ip
          regs = regsPre & _r ipReg .~ ip
          getVal vm i = case vm of
            Reg ->
              regs ^. _r (resolveReg i)
            Imm -> i

          -- category0 covers ops with prefix add / mul / ban / bor
          cat0 mb f =
            let v0 = getVal Reg a
                v1 = getVal mb b
                rOut = resolveReg c
             in regs & _r rOut .~ f v0 v1
          -- category1 covers ops with prefix gt / eq
          cat1 mab f = do
            let (ma, mb) = case mab of
                  ImmReg -> (Imm, Reg)
                  RegImm -> (Reg, Imm)
                  RegReg -> (Reg, Reg)
                v0 = getVal ma a
                v1 = getVal mb b
                rOut = resolveReg c
             in regs & _r rOut .~ bool 0 1 (f v0 v1)
          regs' :: (Int, Int, Int, Int, Int, Int)
          regs' = case sOp of
            Add mb -> cat0 mb (+)
            Mul mb -> cat0 mb (*)
            BitAnd mb -> cat0 mb (.&.)
            BitOr mb -> cat0 mb (.|.)
            Assign ma ->
              let v0 = getVal ma a
                  rOut = resolveReg c
               in regs & _r rOut .~ v0
            TestGreaterThan mab -> cat1 mab (>)
            TestEqual mab -> cat1 mab (==)
          ip' :: Int
          ip' = regs' ^. _r ipReg
       in Just (ip' + 1, regs')
  where
    _r = \case
      R0 -> _1'
      R1 -> _2'
      R2 -> _3'
      R3 -> _4'
      R4 -> _5'
      R5 -> _6'

    resolveReg :: Int -> Register
    resolveReg i =
      toEnum @Register i

runProgram :: Program -> Machine
runProgram prog = runAux (0, (0, 0, 0, 0, 0, 0))
  where
    runAux :: Machine -> Machine
    runAux m = traceShow m $ case interpret prog m of
      Nothing -> m
      Just m' -> runAux m'

runProgram2 :: Program -> Machine
runProgram2 prog = runAux (0, (1, 0, 0, 0, 0, 0))
  where
    runAux :: Machine -> Machine
    runAux m = traceShow m $ case interpret prog m of
      Nothing -> m
      Just m' -> runAux m'

instance Solution Day19 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    prog@(r, xs) <- consumeOrDie programP <$> getInputS
    -- print (runProgram2 prog)
    mapM_ (\(i, instr) -> putStrLn $ show i <> "\t" <> pprInstr r instr) (zip [0 :: Int ..] $ V.toList xs)
    print (factorise (10551287 :: Int))
    -- https://math.stackexchange.com/a/22723/139439
    answerShow $ (1 + 127) * (1 + 251) *(1 + 331)
