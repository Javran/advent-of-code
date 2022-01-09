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
import Debug.Trace
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2018.Day16 (BinValueMode (..), OpType (..), ValueMode (..))
import Math.NumberTheory.Primes
import Text.ParserCombinators.ReadP hiding (count, get, many)
import Text.Printf

data Day19 deriving (Generic)

type Tuple6 a = (a, a, a, a, a, a)

type Regs = Tuple6 Int

instrP :: ReadP Instr
instrP = do
  opName <- munch1 isAlpha
  Just sOp <- pure (ops M.!? opName)
  ~[a, b', c] <- replicateM 3 (char ' ' *> readS_to_P (reads @Int))
  let b = case sOp of
        Assign _ ->
          {-
            TODO: smart constructor.

            for seti / setr, b is completely ignored.
            we can normalize it to a fixed value so that we don't
            need to treat this as a special case, and Eq can be derived.
           -}
          0
        _ -> b'
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
  deriving (Show, Eq)

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

type Breakpoints = IS.IntSet

_reg :: Register -> Lens' Regs Int
_reg = \case
  R0 -> _1'
  R1 -> _2'
  R2 -> _3'
  R3 -> _4'
  R4 -> _5'
  R5 -> _6'

interpret :: Program -> Breakpoints -> Machine -> Maybe Machine
interpret (ipReg, instrs) breakpoints (ip, regsPre) =
  if ip < 0 || ip >= V.length instrs || IS.member ip breakpoints
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
       in Just (ip' + 1, regs')
  where
    resolveReg :: Int -> Register
    resolveReg i =
      toEnum @Register i

runProgram :: Program -> Breakpoints -> Regs -> Machine
runProgram prog bps inp = runAux (0, inp)
  where
    runAux :: Machine -> Machine
    runAux m = case interpret prog bps m of
      Nothing -> m
      Just m' -> runAux m'

-- https://math.stackexchange.com/a/22723/139439
sumOfProperDivisors :: Int -> Int
sumOfProperDivisors = product . fmap f . factorise
  where
    f (p, m) = sum (take (1 + fromIntegral m) $ iterate (* unPrime p) 1)

staticAnalysis :: Program -> Either String Register
staticAnalysis (ipReg, prog) = do
  let ip = fromEnum ipReg
  unless (V.length prog == 36) do
    Left "unexpected program length"
  unless (prog V.! 26 == (Instr (Assign Imm) (0, 0, ip))) do
    Left "expected jump to 0 at 26"
  unless (prog V.! 35 == (Instr (Assign Imm) (0, 0, ip))) do
    Left "expected jump to 0 at 35"
  unless (prog V.! 16 == (Instr (Mul Reg) (ip, ip, ip))) do
    Left "expected halt at 16"
  unless (sOp (prog V.! 4) == TestEqual RegReg) do
    Left "expected eqrr at 4"
  let (_, inpReg, _) = sOperands (prog V.! 4)
  pure (toEnum inpReg)

extractInput :: Program -> Int -> Int
extractInput prog r0 = m ^. _reg inpReg
  where
    (1, m) = runProgram prog (IS.singleton 1) (r0, 0, 0, 0, 0, 0)
    inpReg = case staticAnalysis prog of
      Right v -> v
      Left msg -> error $ "static analysis failed: " <> msg

instance Solution Day19 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow, terminal} = do
    (extraOps, rawInput) <- consumeExtraLeadingLines <$> getInputS
    let prog@(r, xs) = consumeOrDie programP rawInput
    case extraOps of
      Nothing -> do
        when (isJust terminal) $
          mapM_ (\(i, instr) -> putStrLn $ show i <> "\t" <> pprInstr r instr) (zip [0 :: Int ..] $ V.toList xs)
        answerShow $ sumOfProperDivisors $ extractInput prog 0
        answerShow $ sumOfProperDivisors $ extractInput prog 1
      Just _ ->
        answerShow $ snd (runProgram prog IS.empty (0, 0, 0, 0, 0, 0))

