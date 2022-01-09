{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module Javran.AdventOfCode.Y2018.Day21
  (
  )
where

import Control.Monad
import Control.Monad.State.Strict
import Data.Bits
import qualified Data.IntSet as IS
import qualified Data.Vector as V
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2018.Day16 (OpType (..), ValueMode (..))
import Javran.AdventOfCode.Y2018.Day19

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
  ;; line 7 seens to be the only varying number across inputs.
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
  20    r4 = r4 > r2
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

  This whole program can be refactored into (in C++):

  void simulate(long int r0) {
    long int r1, r2;
    r1 = 0;
    do {
      r2 = r1 | 0x1'0000;
      r1 = 10605201; // the only varying line.
      for (;;) {
        r1 = r1 + (r2 & 0xFF);
        r1 &= 0xFF'FFFF;
        r1 = r1 * 65899;
        r1 &= 0xFF'FFFF;
        if (r2 >= 256) {
          r2 /= 256;
        } else {
          break;
        }
      }
      // all we need is r1 value at this point.
    } while (r1 != r0);
  }

  Part 1: simulate this function, see what we have in r1, which is our answer.
  Part 2: due to masking, 0 <= r1 <= 0xFF_FFFF, meaning we got to find r1 value looping.
    To make the longest loop is to find the r1 value prior to running into the loop.
 -}

fastSimulate :: Int -> Int -> Int
fastSimulate c v = evalState sim (c, v .|. 0x1_0000)
  where
    sim :: State (Int, Int) Int
    sim = do
      modify (\(r1, r2) -> ((r1 + (r2 .&. 0xFF)) .&. 0xFF_FFFF, r2))
      modify (first \r1 -> r1 * 65899 .&. 0xFF_FFFF)
      (r1, r2) <- get
      if r2 >= 256
        then do
          modify (second (`quot` 256))
          sim
        else pure r1

findFix :: IS.IntSet -> [(Int, Int)] -> Int
findFix seen ~((x, y) : xs) =
  if IS.member y seen
    then -- note that here we need the value right before running into a loop.
      x
    else findFix (IS.insert y seen) xs

instance Solution Day21 where
  solutionRun _ SolutionContext {getInputS, answerShow, terminal} = do
    prog <- consumeOrDie programP <$> getInputS
    let xs = tail $ iterate (fastSimulate a) 0
        Instr {sOp = Assign Imm, sOperands = (a, _, _)} = snd prog V.! 7
    when (isJust terminal) do
      pprProgram prog
    answerShow $ head xs
    answerShow $ findFix IS.empty (zip xs (tail xs))
