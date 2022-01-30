{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2016.Day25
  (
  )
where

import Control.Monad
import Data.Char
import qualified Data.Vector as V
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2016.Day12 (ReadVal, Reg (..), readValP, regP)
import Text.ParserCombinators.ReadP hiding (count, get, many)
import Text.Printf
import Javran.AdventOfCode.Misc (commitLeft1)

data Day25 deriving (Generic)

data Instr
  = Cpy ReadVal Reg
  | Inc Reg
  | Dec Reg
  | Jnz ReadVal ReadVal
  | Out ReadVal
  deriving (Show)

instrP :: ReadP Instr
instrP =
  commitLeft1
    [ binary "cpy" Cpy readValP regP
    , unary "inc" Inc regP
    , unary "dec" Dec regP
    , binary "jnz" Jnz readValP readValP
    , unary "out" Out readValP
    ]
  where
    sp = char ' '
    unary lit builder pa =
      string lit *> sp *> (builder <$> pa)
    binary lit builder pa pb =
      unary lit builder pa <*> (sp *> pb)

pprInstrs :: V.Vector Instr -> IO ()
pprInstrs vs = mapM_ pprInstr (zip [0 ..] $ V.toList vs)
  where
    pprReg (Reg i) = [chr (ord 'a' + i)]
    pprReadVal = \case
      Left i -> show i
      Right r -> pprReg r
    pprToLoc pc r = case r of
      Left offset -> "to loc " <> show (pc + offset)
      Right reg -> " to loc (" <> pprReg reg <> " + " <> show pc <> ")"
    pprInstr :: (Int, Instr) -> IO ()
    pprInstr (pc, instr) =
      putStrLn $
        lineNum <> case instr of
          Out x ->
            "out " <> pprReadVal x
          Inc reg ->
            pprReg reg <> " += 1"
          Dec reg ->
            pprReg reg <> " -= 1"
          Cpy x reg ->
            pprReg reg <> " = " <> pprReadVal x
          Jnz (Left v) y ->
            if v == 0
              then "nop"
              else "jmp " <> pprToLoc pc y
          Jnz x y ->
            "jnz " <> pprReadVal x <> " " <> pprToLoc pc y
      where
        lineNum :: String
        lineNum = printf "  %2d:  " pc

{-
  Got the following instructions on my specific input:

   0:  d = a
   1:  c = 4
   2:  b = 643
   3:  d += 1
   4:  b -= 1
   5:  jnz b to loc 3
   6:  c -= 1
   7:  jnz c to loc 2
  ;; now we have d = a + 643 * 4.

  ;; Note that nothing beyond this point mutates register d,
  ;; in fact only loc 8~29 is reachable.
  ;; further, loc 8 is only reached when a == 0,
  ;; suggesting that we are looking at a value `v`,
  ;; where d = v + 643 * 4, this is then "re-supplied to register a"
  ;; for this infinite loop to continue.
  ;; In other words, we are looking for a `d` value that can give us
  ;; one part of the clock signal, which is then repeated to
  ;; achieve infinite generation.

   8:  a = d
   9:  nop
  10:  b = a
  11:  a = 0
  12:  c = 2
  13:  jnz b to loc 15
  14:  jmp to loc 20
  15:  b -= 1
  16:  c -= 1
  17:  jnz c to loc 13
  18:  a += 1
  ;; we have (a, c) = d `quotRem` 2 here.
  19:  jmp to loc 12
  20:  b = 2
  21:  jnz c to loc 23
  22:  jmp to loc 26
  23:  b -= 1
  24:  c -= 1
  25:  jmp 1 to loc 21
  26:  nop
  27:  out b
  28:  jnz a to loc 9
  29:  jmp to loc 8

  In short, we need to construct a value for register `a`,
  which when added to 643 * 4 would produce a value
  whose binary representation is (from LSB to MSB) 0,1 repeated at least once.

  Further examination of other inputs reveals that the only varying parts are
  the number assigned to `c` at loc 1, and number assigned to `b` at loc 2
  - which are the only input values we need to solve this problem.

 -}

solve :: Int -> Int -> Int
solve c b = target - n
  where
    n = c * b
    target : _ =
      dropWhile (< n) $
        {-
          This generates binary representation {0,1} from LSB to MSB
          repeated indefinitely (or as long as the carrying type can store).
         -}
        iterate (\v -> v * 4 + 2) 0

instance Solution Day25 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    instrs <- V.fromList . fmap (consumeOrDie instrP) . lines <$> getInputS
    let Cpy (Left c) (Reg 2) = instrs V.! 1
        Cpy (Left b) (Reg 1) = instrs V.! 2
        showInstrs = False
    when showInstrs do
      pprInstrs instrs
    answerShow $ solve c b
