{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module Javran.AdventOfCode.Y2016.Day23
  (
  )
where


import Control.Monad
import Control.Monad.ST
import Data.Char
import Data.Function
import Data.List
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Debug.Trace
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)
import Text.Printf
import Javran.AdventOfCode.TestExtra (consumeExtra)

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

{-
  For solving part2, the actual clue comes from dumping registers right before
  executing the instruction at pc=16, which is a `tgl` instruction.
  Set `dumpRegs` below to True to observe this in action:

  any non-negative aVale less than 6 seems to stuck, so we begin with 6:
  (the output format below `(loc, regs)` means `loc` will be toggled,
  and prior to the toggling, registers are `regs`, in that order).

  aVal = 6:

  (24,[30,4,8,0])
  (22,[120,3,6,0])
  (20,[360,2,4,0])
  (18,[720,1,2,0])
  final a = 7158

  aVal = 7:

  (24,[210,4,8,0])
  (22,[840,3,6,0])
  (20,[2520,2,4,0])
  (18,[5040,1,2,0])
  final a = 11478

  aVal = 8:
  (24,[1680,4,8,0])
  (22,[6720,3,6,0])
  (20,[20160,2,4,0])
  (18,[40320,1,2,0])
  final a = 46758

  aVal = 9:
  (24,[15120,4,8,0])
  (22,[60480,3,6,0])
  (20,[181440,2,4,0])
  (18,[362880,1,2,0])
  final a = 369318

  aVal = 10:
  (24,[151200,4,8,0])
  (22,[604800,3,6,0])
  (20,[1814400,2,4,0])
  (18,[3628800,1,2,0])
  final a = 3635238

  now it's easier to see that:

  - toggle always happen at location 24, 22, 20, and 18, in that order,
    regardless of what the input value is.

  - right before loc 18 is toggled (this is also the last time we run a toggle instruction),
    reg a is `product [1..aVal]`, and reg b, c, d are [1,2,0], in that order.

  Since the only state for this virtual machine is pc and registers,
  we can re-construct the state right before entering loc 16 (i.e. the tgl instruction)
  for the last time, and allow the machine to continue from there.

 -}
interpret :: forall s. [Instr] -> Bool -> Int -> ST s Int
interpret instrSrc fastForward aVal = do
  let dumpRegs = False
  regs <- VM.replicate 4 (0 :: Int)
  -- setup for login input.
  VM.unsafeWrite regs 0 aVal
  instrs <- V.unsafeThaw $ V.fromList instrSrc
  let getVal = \case
        Left i -> pure i
        Right (Reg i) -> VM.unsafeRead regs i

  when fastForward do
    VM.unsafeModify instrs toggleInstr 24
    VM.unsafeModify instrs toggleInstr 22
    VM.unsafeModify instrs toggleInstr 20
    VM.unsafeWrite regs 0 $! product [1 .. aVal]
    VM.unsafeWrite regs 1 1
    VM.unsafeWrite regs 2 2
    VM.unsafeWrite regs 3 0

  let startPc = if fastForward then 16 else 0

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
                      mayTrace <-
                        if dumpRegs
                          then do
                            curRegs <- V.freeze regs
                            pure (traceShow (ind, curRegs))
                          else pure id
                      VM.unsafeModify instrs toggleInstr $ mayTrace ind
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
    startPc

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
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    let showInstrs = False
    (extraOp, rawInput) <- consumeExtra getInputS
    let instrs = fmap (consumeOrDie instrP) . lines $ rawInput
    when showInstrs do
      pprInstrs $ V.fromList instrs

    case extraOp of
      Nothing -> do
        answerShow $ runST $ interpret instrs True 7
        answerShow $ runST $ interpret instrs True 12
      Just _ ->
        answerShow $ runST $ interpret instrs False 0
