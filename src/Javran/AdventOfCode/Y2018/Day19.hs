{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2018.Day19
  ( Program
  , Regs
  , programP
  , Instr (..)
  , pprProgram
  , Machine
  , Register(..)
  , _reg
  )
where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Bits
import Data.Char
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2018.Day16 (BinValueMode (..), OpType (..), ValueMode (..))
import Math.NumberTheory.Primes
import Text.ParserCombinators.ReadP hiding (count, get, many)
import Text.Printf

data Day19 deriving (Generic)

type Tuple6 a = (a, a, a, a, a, a)

type Regs = Tuple6 Int

data Instr = Instr
  { sOp :: OpType
  , sOperands :: (Int, Int, Int)
  }
  deriving (Show, Eq)

mkInstr :: OpType -> (Int, Int, Int) -> Instr
mkInstr sOp (a, bPre, c) = Instr {sOp, sOperands = (a, b, c)}
  where
    b = case sOp of
      Assign _ ->
        {-
          for seti / setr, b is completely ignored.
          we can normalize it to a fixed value so that we don't
          need to treat this as a special case, and thus Eq can be derived.
        -}
        0
      _ -> bPre

instrP :: ReadP Instr
instrP = do
  opName <- munch1 isAlpha
  Just sOp <- pure (ops M.!? opName)
  ~[a, b, c] <- replicateM 3 (char ' ' *> readS_to_P (reads @Int))
  pure $ mkInstr sOp (a, b, c)

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

pprInstr :: Register -> Int -> Instr -> String
pprInstr ipReg curIp Instr {sOp, sOperands = (a, b, c)} = case sOp of
  Add m -> ppr0 "+" m
  Mul m -> ppr0 "*" m
  BitAnd m -> ppr0 "&" m
  BitOr m -> ppr0 "|" m
  Assign m -> pprAux c (modeIn a m)
  TestEqual ms -> ppr1 "==" ms
  TestGreaterThan ms -> ppr1 ">" ms
  where
    modeOut x = \case
      Reg -> if x == fromEnum ipReg then "ip" else "r" <> show x
      Imm -> show x
    modeIn x = \case
      Reg ->
        if x == fromEnum ipReg
          then {-
                 whenever we see ip register as one of the input value,
                 it's just a constant value of that line.
                -}
            show curIp
          else "r" <> show x
      Imm -> show x
    pprAux outReg rhs =
      if outReg == fromEnum ipReg
        then {-
               assigning to ip register is just a jump,
               also `plus one` is attached since in this model
               ip always advance regardless of whether
               we are assigning to it.
              -}
          "jmp (" <> rhs <> ")+1"
        else modeOut outReg Reg <> " = " <> rhs
    ppr0 opStr m =
      pprAux c (unwords [modeIn a Reg, opStr, modeIn b m])
    ppr1 opStr ms =
      pprAux c (unwords [modeIn a m1, opStr, modeIn b m2])
      where
        (m1, m2) = case ms of
          ImmReg -> (Imm, Reg)
          RegImm -> (Reg, Imm)
          RegReg -> (Reg, Reg)

{-
  Pretty prints the program in a way that helps readability.
 -}
pprProgram :: Program -> IO ()
pprProgram (r, xs) = do
  putStrLn $ "IP is bound to " <> show r
  let maxWidth = length $ show (V.length xs - 1)
  mapM_
    (\(i, instr) -> printf "  %*d    %s\n" maxWidth i (pprInstr r i instr))
    (zip [0 :: Int ..] $ V.toList xs)

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
    runAux m = maybe m runAux (interpret prog bps m)

-- https://math.stackexchange.com/a/22723/139439
sumOfProperDivisors :: Int -> Int
sumOfProperDivisors = product . fmap f . factorise
  where
    f (p, m) = sum (take (1 + fromIntegral m) $ iterate (* unPrime p) 1)

{-
  Analyzes program without running it, returns information regarding
  how should we obtain the input value of a sum-of-proper-divisors problem.
 -}
staticAnalysis :: Program -> Either String Register
staticAnalysis (ipReg, prog) = do
  let ip = fromEnum ipReg
  unless (V.length prog == 36) do
    Left "unexpected program length"
  unless (prog V.! 26 == mkInstr (Assign Imm) (0, 0, ip)) do
    Left "expected jump to 0 at 26"
  unless (prog V.! 35 == mkInstr (Assign Imm) (0, 0, ip)) do
    Left "expected jump to 0 at 35"
  unless (prog V.! 16 == mkInstr (Mul Reg) (ip, ip, ip)) do
    Left "expected halt at 16"
  unless (sOp (prog V.! 4) == TestEqual RegReg) do
    Left "expected eqrr at 4"
  let (_, inpReg, _) = sOperands (prog V.! 4)
  pure (toEnum inpReg)

{-
  The following is what I have after analyzing my login input:

  ;; note that `jmp` and `halt` are made up,
  ;; though their meanings should be obvious.
  ;;
  ;; r1 appears to be a temporary result register.
  ;; r0 is the "answer register", the only mutation to it at 7,
  ;; which means it accumulates from r3.

  0     jmp 17
  1     r3 = 1
  2     r5 = 1
  3     r1 = r3 * r5
  4     r1 = r1 == r2
  5     jmp r1 + 6
  ;; when r3 * r5 /= r2
  6     jmp 8
  ;; otherwise
  7     r0 = r3 + r0 ;; only do this when r3 * r5 == r2
  8     r5 = r5 + 1
  9     r1 = r5 >= r2
  10    jmp r1 + 11
  ;; when r5 < r2
  11    jmp 3
  ;; otherwise
  12    r3 = r3 + 1
  13    r1 = r3 >= r2
  14    jmp r1 + 15
  ;; when r3 < r2
  15    jmp 2
  ;; otherwise
  16    halt
  ;; everything beyond this appears to be input setup
  ;; in fact:
  ;; - r0 is always 0 when we jump back to 1.
  ;; - whatever value r1 holds doesn't matter, as it's overwritten at 3.
  ;; - r2 is never mutated above.
  17    r2 = r2 + 2
  18    r2 = r2 * r2
  19    r2 = 19 * r2
  20    r2 = r2 * 11
  21    r1 = r1 + 2
  22    r1 = r1 * 22
  23    r1 = r1 + 7
  24    r2 = r2 + r1
  25    jmp r0 + 26
  26    jmp 1  ;; r2 = 887
  27    r1 = 27
  28    r1 = r1 * 28
  29    r1 = 29 + r1
  30    r1 = 30 * r1
  31    r1 = r1 * 14
  32    r1 = r1 * 32
  33    r2 = r2 + r1
  34    r0 = 0
  35    jmp 1  ;; r2 = 10551287

  So, what we are looking at is a program that does the following:

  > sum [ r3 | r3 <- [1..r2-1], r5 <- [1..r2-1], r3 * r5 == r2 ]

  In other words, we are computing sum of proper divisors of some input n,
  in our case the input value is held in register r2.

  For solving my specific input, this analysis is sufficient to give us the right answer.

  For a general solution, I think it's safe to assume the following,
  after having obtained some other login input data:

  - the program always consists of 36 instructions
    + line 0~16 is the main body of algorithm
    + line 17~35 prepares input number to a register and jumps back to line 1

  - the only line that uses input value in the main body (line 0~16) is at 4
    + which is an `eqrr` instruction, its second operand (RHS of `==`) holds the input value.

  - when the program arrives at line 1, the input value is computed
    and will never be mutated after.

  The function `staticAnalysis` above verifies those assumptions and looks at line 4
  to figure out which register is used to store the input value.

  We let the program run as-is, but stop at line 1.
  At this point, the input to a sum-of-proper-divisors problem is in some register,
  which `staticAnalysis` should be able to figure out.
  Then we can use a faster algorithm to solve the problem,
  instead of allowing this slow program to run to its termination.
  This whole logic of obtaining the input value is implemented as `extractInput`.
 -}
extractInput :: Program -> Int -> Int
extractInput prog r0 = m ^. _reg inpReg
  where
    (1, m) = runProgram prog (IS.singleton 1) (r0, 0, 0, 0, 0, 0)
    inpReg = case staticAnalysis prog of
      Right v -> v
      Left msg -> error $ "static analysis failed: " <> msg

instance Solution Day19 where
  solutionRun _ SolutionContext {getInputS, answerShow, terminal} = do
    (extraOps, rawInput) <- consumeExtraLeadingLines <$> getInputS
    let prog = consumeOrDie programP rawInput
    case extraOps of
      Nothing -> do
        when (isJust terminal) $
          pprProgram prog
        answerShow $ sumOfProperDivisors $ extractInput prog 0
        answerShow $ sumOfProperDivisors $ extractInput prog 1
      Just _ ->
        -- just dump all registers as output if we are running examples.
        answerShow $ snd (runProgram prog IS.empty (0, 0, 0, 0, 0, 0))
