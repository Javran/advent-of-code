{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2017.Day23
  (
  )
where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Writer.CPS
import Data.Char
import qualified Data.IntSet as IS
import Data.List
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Javran.AdventOfCode.Prelude
import Math.NumberTheory.Primes.Testing
import Text.ParserCombinators.ReadP hiding (count, get, many)
import Text.Printf

data Day23 deriving (Generic)

data Reg = Reg Int deriving (Show)

mkReg :: Char -> Reg
mkReg ch = Reg $ ord ch - ord 'a'

-- immediate value or a value from register
type ReadVal = Either Int Reg

data Instr
  = Assign Reg ReadVal
  | Sub Reg ReadVal
  | Mul Reg ReadVal
  | JumpNotZero ReadVal ReadVal
  deriving (Show)

instrP :: ReadP Instr
instrP =
  foldl1'
    (<++)
    [ binary "set" Assign regP readValP
    , binary "sub" Sub regP readValP
    , binary "mul" Mul regP readValP
    , binary "jnz" JumpNotZero readValP readValP
    ]
  where
    sp = char ' '
    unary lit builder pa =
      string lit *> sp *> (builder <$> pa)
    binary lit builder pa pb =
      unary lit builder pa <*> (sp *> pb)

    -- below are basic parsers that won't consume extra spaces afterwards.
    intP = readS_to_P (reads @Int)
    regP = mkReg <$> satisfy (\ch -> ch >= 'a' && ch <= 'h')
    readValP = (Right <$> regP) <++ (Left <$> intP)

interpret :: forall s. V.Vector Instr -> WriterT (Sum Int) (ST s) ()
interpret instrs = do
  regs <- VM.replicate 8 0
  let getVal = \case
        Left v -> pure v
        Right (Reg i) -> lift $ VM.unsafeRead regs i
      liftOp op rx@(Reg x) y = do
        a <- getVal (Right rx)
        b <- getVal y
        lift $ VM.unsafeWrite regs x $ op a b
  fix
    (\go pc ->
       if pc < 0 || pc >= V.length instrs
         then pure ()
         else case instrs V.! pc of
           Assign rx y -> do
             liftOp (\_ y' -> y') rx y
             go (pc + 1)
           Sub rx y -> do
             liftOp (-) rx y
             go (pc + 1)
           Mul rx y -> do
             tell 1
             liftOp (*) rx y
             go (pc + 1)
           JumpNotZero x y -> do
             x' <- getVal x
             y' <- getVal y
             go (if x' /= 0 then pc + y' else pc + 1))
    0

pprInstrs :: V.Vector Instr -> IO ()
pprInstrs instrs =
  forM_ paired \(pc, instr) -> do
    printf
      "%c %4d:  %s\n"
      (if IS.member pc jumpTargets then '>' else ' ')
      pc
      (pprInstr pc instr)
  where
    paired = zip [0 :: Int ..] (V.toList instrs)
    jumpTargets = IS.fromList do
      (pc, JumpNotZero _ (Left v)) <- paired
      pure (pc + v)
    pprReadVal :: ReadVal -> String
    pprReadVal = \case
      Left v -> show v
      Right r -> pprReg r
    pprReg :: Reg -> String
    pprReg (Reg v) = [chr (v + ord 'a')]
    pprInstr :: Int -> Instr -> String
    pprInstr pc instr = case instr of
      Assign l r ->
        pprReg l <> " = " <> pprReadVal r
      Mul l r ->
        pprReg l <> " = " <> pprReg l <> " * " <> pprReadVal r
      Sub l (Left v)
        | v < 0 ->
          pprReg l <> " = " <> pprReg l <> " + " <> pprReadVal (Left (negate v))
      Sub l r ->
        pprReg l <> " = " <> pprReg l <> " - " <> pprReadVal r
      JumpNotZero (Right r) (Left v) -> "jnz " <> pprReg r <> " " <> show (pc + v)
      JumpNotZero (Left 1) (Left v) ->
        let pc' = pc + v
         in if pc' < 0 || pc' >= V.length instrs
              then "halt"
              else "goto " <> show pc'
      _ -> show instr

{-
  My login output when pretty-printed ('>' indicates a jump target):

       0:  b = 79
       1:  c = b
       2:  jnz a 4
       3:  goto 8
  >    4:  b = b * 100
       5:  b = b + 100000
       6:  c = b
       7:  c = c + 17000
  >    8:  f = 1
       9:  d = 2
  >   10:  e = 2
  >   11:  g = d
      12:  g = g * e
      13:  g = g - b
      14:  jnz g 16
      15:  f = 0
  >   16:  e = e + 1
      17:  g = e
      18:  g = g - b
      19:  jnz g 11
      20:  d = d + 1
      21:  g = d
      22:  g = g - b
      23:  jnz g 10
      24:  jnz f 26
      25:  h = h + 1
  >   26:  g = b
      27:  g = g - c
      28:  jnz g 30
      29:  halt
  >   30:  b = b + 17
      31:  goto 8

simlifies to (in C++):

  int simulate() {
    int b(0), c(0), d(0);
    int e(0), f(0), h(0);

    b = 107900;
    c = b + 17000;

    for (;;) {
      f = 1;
      d = 2;
      do {
        e = 2;
        do {
          if (d * e == b) {
            f = 0;
          }
          e = e + 1;
        } while (e < b);
        d = d + 1;
      } while (d < b);

      if (f == 0) {
        h = h + 1;
      }
      if (b == c) {
        return h;
      }
      b = b + 17;
    }
  }

  So, in pseudo code:

  bInit = 107900
  b <- [bInit, bInit + 17 .. bInit + 17000]
  f = 1
  if there's any:
    e <- [2 .. b - 1]
    d <- [2 .. b - 1]
    such that e * d == b
    f = 0

  if f == 0:
    h += 1

  In other words, we are counting non-primes.

  Update: after getting few other input samples,
  it seems like the only varying number is b.

 -}
solve :: Int -> Int
solve inputB =
  countLength (not . isPrime . fromIntegral) [b, b + 17 .. b + 17000]
  where
    b = 100000 + inputB * 100

instance Solution Day23 where
  solutionRun _ SolutionContext {getInputS, answerShow, terminal} = do
    instrs <- V.fromList . fmap (consumeOrDie instrP) . lines <$> getInputS
    let (Assign (Reg 1) (Left inp)) = V.head instrs
        part1ByInterpret = False
        prettyPrintInstrs = False
    when (isJust terminal && prettyPrintInstrs) do
      pprInstrs instrs

    answerShow $
      if part1ByInterpret
        then getSum $ runST $ execWriterT $ interpret instrs
        else {-
               with the hindsight from part 2,
               we know we are just testing every pair in range [2 .. inp-1],
               therefore the same amount of `mul` instructions.
              -}
          let v = inp - 2 in v * v
    answerShow $ solve inp
