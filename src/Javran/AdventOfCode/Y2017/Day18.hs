{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Javran.AdventOfCode.Y2017.Day18
  (
  )
where

import Control.Monad
import Control.Monad.Writer.CPS
import Data.Char
import qualified Data.IntMap.Strict as IM
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import Data.Void
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.TestExtra
import Text.ParserCombinators.ReadP hiding (count, get, many)
import Javran.AdventOfCode.Misc (commitLeft1)

data Day18 deriving (Generic)

data Reg = Reg Int deriving (Show)

mkReg :: Char -> Reg
mkReg ch = Reg $ ord ch - ord 'a'

-- immediate value or a value from register
type ReadVal = Either Int Reg

data Instr
  = Send ReadVal
  | Assign Reg ReadVal
  | Add Reg ReadVal
  | Mul Reg ReadVal
  | Mod Reg ReadVal
  | Recv Reg
  | JumpGtZero ReadVal ReadVal
  deriving (Show)

instrP :: ReadP Instr
instrP =
  commitLeft1
    [ unary "snd" Send readValP
    , binary "set" Assign regP readValP
    , binary "add" Add regP readValP
    , binary "mul" Mul regP readValP
    , binary "mod" Mod regP readValP
    , unary "rcv" Recv regP
    , binary "jgz" JumpGtZero readValP readValP
    ]
  where
    sp = char ' '
    unary lit builder pa =
      string lit *> sp *> (builder <$> pa)
    binary lit builder pa pb =
      unary lit builder pa <*> (sp *> pb)

    -- below are basic parsers that won't consume extra spaces afterwards.
    intP = readS_to_P (reads @Int)
    regP = mkReg <$> satisfy isAsciiLower
    readValP = (Right <$> regP) <++ (Left <$> intP)

type Regs = IM.IntMap Int

{- ((pc, regs), last sent) -}
type Machine = ((Int, Regs), Maybe Int)

valGetter :: Regs -> ReadVal -> Int
valGetter regs = \case
  Left v -> v
  Right (Reg i) -> fromMaybe 0 (regs IM.!? i)

updateRegsWith :: (Int -> Int -> Int) -> Reg -> ReadVal -> Regs -> Regs
updateRegsWith op rx@(Reg x) y regs =
  IM.insert x (op (getVal (Right rx)) (getVal y)) regs
  where
    getVal = valGetter regs

interpret :: V.Vector Instr -> Machine -> Maybe Machine
interpret instrs ((pc, regs), lastSent) = do
  when (pc < 0 || pc >= V.length instrs) do Nothing
  let getVal = valGetter regs
      liftOp op rx y =
        ( ( pc + 1
          , updateRegsWith op rx y regs
          )
        , lastSent
        )
  pure case instrs V.! pc of
    Send x -> ((pc + 1, regs), Just (getVal x))
    Assign rx y -> liftOp (\_ y' -> y') rx y
    Add rx y -> liftOp (+) rx y
    Mul rx y -> liftOp (*) rx y
    Mod rx y -> liftOp rem rx y
    Recv rx ->
      if getVal (Right rx) == 0
        then ((pc + 1, regs), lastSent)
        else liftOp (\_ _ -> fromJust lastSent) rx (error "unused")
    JumpGtZero x y ->
      let x' = getVal x
          y' = getVal y
       in ((pc + if x' > 0 then y' else 1, regs), lastSent)

solve :: V.Vector Instr -> Machine -> Int
solve instrs st@((pc, _), _) = case interpret instrs st of
  Nothing -> error "no recover instr"
  Just st'@(_, sent) -> case instrs V.! pc of
    Recv _ -> fromMaybe (error "nothing is sent") sent
    _ -> solve instrs st'

-- below are for part 2.

{-
  machinery for tagging results with either internal or external.
 -}
data ExtInt = External | Internal

type family InternalOnly (ei :: ExtInt) where
  InternalOnly 'Internal = ()
  InternalOnly 'External = Void

data Result (ei :: ExtInt)
  = -- | Done with execution
    Done Machine2
  | -- | Machine is paused but can be resumed without affecting input / output
    Paused (InternalOnly ei) (Result ei)
  | -- | Needs an input value to proceed
    NeedInput (Int -> Result ei)
  | -- | An output value is sent and machine itself paused
    SentOutput Int (Result ei)

type Machine2 = (Int, Regs)

start :: V.Vector Instr -> Machine2 -> Result 'Internal
start instrs = fix \go m@(pc, regs) ->
  if pc < 0 || pc >= V.length instrs
    then Done m
    else
      let getVal = valGetter regs
          liftOp op rx y =
            Paused () $ go (pc + 1, updateRegsWith op rx y regs)
       in case instrs V.! pc of
            Send x -> SentOutput (getVal x) $ go (pc + 1, regs)
            Assign rx y -> liftOp (\_ y' -> y') rx y
            Add rx y -> liftOp (+) rx y
            Mul rx y -> liftOp (*) rx y
            Mod rx y -> liftOp rem rx y
            Recv (Reg r) -> NeedInput \inp ->
              go (pc + 1, IM.insert r inp regs)
            JumpGtZero x y ->
              Paused () $ go (pc + if getVal x > 0 then getVal y else 1, regs)

{-
  Keeps the machine ticking until it's done or would affect input / output.
  By doing so we eliminated the `Paused` internal state from external API.
 -}
keepGoing :: Result 'Internal -> Result 'External
keepGoing = \case
  Done m -> Done m
  Paused () k -> keepGoing k
  NeedInput k -> NeedInput (keepGoing . k)
  SentOutput o k -> SentOutput o (keepGoing k)

type ResultWithMsgQueue = (Result 'External, Seq.Seq Int)

type System = (ResultWithMsgQueue, ResultWithMsgQueue)

{-
  Manually ticks the external system as a whole, and keeps track of
  how many times did program 1 send an output.

  Notes:

  It's a bit on the noisy side, but my intention is to implement
  this duet system in a way that it reads like a machine proof.

  Now it's easy to see that:
  - at first we deal with impossible cases
    as those are eliminated from external API.
  - the only work for this impl is to carry one's output to another's input.
  - all the other cases are either full system termination or deadlock situation.

 -}
runDuet :: System -> Writer (Sum Int) ()
runDuet = \case
  ((Paused v _, _), _) -> absurd v
  (_, (Paused v _, _)) -> absurd v
  ((SentOutput o r0', q0), (r1', q1)) ->
    -- sending output from p0 to p1's msg queue
    runDuet ((r0', q0), (r1', q1 Seq.|> o))
  ((r0', q0), (SentOutput o r1', q1)) ->
    -- increase counter, sending output from p1 to p0's msg queue
    tell 1 >> runDuet ((r0', q0 Seq.|> o), (r1', q1))
  ((NeedInput k0, i Seq.:<| q0'), st1) ->
    -- feed p0 input if its msg queue isn't empty
    runDuet ((k0 i, q0'), st1)
  (st0, (NeedInput k1, i Seq.:<| q1')) ->
    -- feed p1 input if its msg queue isn't empty
    runDuet (st0, (k1 i, q1'))
  ((Done {}, _), (Done {}, _)) ->
    -- both are done, system terminated.
    pure ()
  ((NeedInput {}, Seq.Empty), (Done {}, _)) ->
    -- p0 starving, with p1 terminated.
    pure ()
  ((Done {}, _), (NeedInput {}, Seq.Empty)) ->
    -- p1 starving, with p0 terminated.
    pure ()
  ((NeedInput {}, Seq.Empty), (NeedInput {}, Seq.Empty)) ->
    -- both needs input but there isn't any, deadlocked.
    pure ()

instance Solution Day18 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (extraOps, rawInput) <- consumeExtra getInputS
    let instrs = V.fromList . fmap (consumeOrDie instrP) . lines $ rawInput
        initSt = ((0, IM.empty), Nothing)
        (runPart1, runPart2) = shouldRun extraOps
    when runPart1 do
      answerShow $ solve instrs initSt
    when runPart2 do
      let Reg p = mkReg 'p'
          mkMachine which =
            (keepGoing (start instrs (0, IM.singleton p which)), Seq.empty)
          initSys :: System
          initSys = (mkMachine 0, mkMachine 1)
          Sum ans = execWriter (runDuet initSys)
      answerShow ans
