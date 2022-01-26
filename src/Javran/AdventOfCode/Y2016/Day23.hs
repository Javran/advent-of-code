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

module Javran.AdventOfCode.Y2016.Day23
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Control.Monad.ST
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
import qualified Data.Vector.Mutable as VM
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)
import Text.Printf
import Debug.Trace

data Day23 deriving (Generic)

data Reg = Reg Int deriving (Show)

mkReg :: Char -> Reg
mkReg ch = Reg $ ord ch - ord 'a'

-- immediate value or a value from register
type ReadVal = Either Int Reg

data InstrUn = Inc | Dec | Tgl deriving (Show)

data InstrBin = Jnz | Cpy deriving (Show)

data Instr
  = InstrUnary InstrUn ReadVal
  | InstrBinary InstrBin ReadVal ReadVal
  deriving (Show)

instrP :: ReadP Instr
instrP =
  foldl1'
    (<++)
    [ unary "inc" (InstrUnary Inc) readValP
    , unary "dec" (InstrUnary Dec) readValP
    , unary "tgl" (InstrUnary Tgl) readValP
    , binary "cpy" (InstrBinary Cpy) readValP readValP
    , binary "jnz" (InstrBinary Jnz) readValP readValP
    ]
  where
    sp = char ' '
    unary lit builder pa =
      string lit *> sp *> (builder <$> pa)
    binary lit builder pa pb =
      unary lit builder pa <*> (sp *> pb)

    intP = readS_to_P (reads @Int)
    regP = mkReg <$> satisfy (\ch -> ch >= 'a' && ch <= 'h')
    readValP = (Right <$> regP) <++ (Left <$> intP)

toggleInstr :: Instr -> Instr
toggleInstr = \case
  InstrUnary i x -> InstrUnary (tglUn i) x
  InstrBinary i x y -> InstrBinary (tglBin i) x y
  where
    tglUn = \case
      Inc -> Dec
      _ -> Inc
    tglBin = \case
      Jnz -> Cpy
      _ -> Jnz

interpret :: forall s. [Instr] -> Int -> ST s Int
interpret instrSrc aVal = do
  regs <- VM.replicate 4 (0 :: Int)
  -- setup for login input.
  VM.unsafeWrite regs 0 aVal
  instrs <- V.unsafeThaw $ V.fromList instrSrc
  let getVal = \case
        Left i -> pure i
        Right (Reg i) -> VM.unsafeRead regs i

  VM.unsafeModify instrs toggleInstr 24
  VM.unsafeModify instrs toggleInstr 22
  VM.unsafeModify instrs toggleInstr 20
  VM.unsafeWrite regs 0 $! product [1..aVal]
  VM.unsafeWrite regs 1 1
  VM.unsafeWrite regs 2 2
  VM.unsafeWrite regs 3 0

  fix
    do
      \go pc ->
        if pc < 0 || pc >= VM.length instrs
          then getVal $ Right (Reg 0)
          else
            VM.unsafeRead instrs pc >>= \case
              InstrUnary i ax -> do
                case i of
                  Inc
                    | Right (Reg x) <- ax ->
                      VM.unsafeModify regs (+ 1) x
                  Dec
                    | Right (Reg x) <- ax ->
                      VM.unsafeModify regs (subtract 1) x
                  Tgl -> do
                    offset <- getVal ax
                    let ind = pc + offset
                    when (ind >= 0 && ind < VM.length instrs) do
                      curRegs <- V.freeze regs
                      VM.unsafeModify instrs toggleInstr $ traceShow (ind, curRegs) $ ind
                  _ -> pure ()
                go (pc + 1)
              InstrBinary i ax ay ->
                case i of
                  Cpy
                    | Right (Reg y) <- ay -> do
                      v <- getVal ax
                      VM.unsafeWrite regs y v
                      go (pc + 1)
                  Jnz -> do
                    cond <- getVal ax
                    offset <- getVal ay
                    go (pc + if cond /= 0 then offset else 1)
                  _ -> go (pc + 1)
    16

pprInstrs :: V.Vector Instr -> IO ()
pprInstrs vs = mapM_ pprInstr (zip [0 ..] $ V.toList vs)
  where
    pprReg (Reg i) = [chr (ord 'a' + i)]
    pprReadVal = \case
      Left i -> show i
      Right r -> pprReg r
    pprInstr :: (Int, Instr) -> IO ()
    pprInstr (pc, instr) =
      putStrLn $
        lineNum <> case instr of
          InstrUnary Inc (Right reg) ->
            pprReg reg <> " += 1"
          InstrUnary Dec (Right reg) ->
            pprReg reg <> " -= 1"
          InstrUnary Tgl (Right reg) ->
            "tgl " <> pprReg reg
          InstrBinary Cpy x (Right reg) ->
            pprReg reg <> " = " <> pprReadVal x
          InstrBinary Jnz x (Left offset) ->
            "jnz " <> pprReadVal x <> " to loc " <> show (pc + offset)
          InstrBinary Jnz x (Right reg) ->
            "jnz " <> pprReadVal x <> " to loc (" <> pprReg reg <> " + " <> show pc <> ")"
          _ -> show instr
      where
        lineNum :: String
        lineNum = printf "  %2d:  " pc

instance Solution Day23 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (consumeOrDie instrP) . lines <$> getInputS
    do
      let vs = V.fromList xs
      pprInstrs vs
      print $ runST $ interpret xs 12

