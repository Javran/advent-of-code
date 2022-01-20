{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2016.Day12
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
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day12 deriving (Generic)

data Reg = Reg Int deriving (Show)

mkReg :: Char -> Reg
mkReg ch = Reg $ ord ch - ord 'a'

-- immediate value or a value from register
type ReadVal = Either Int Reg

data Instr
  = Cpy ReadVal Reg
  | Inc Reg
  | Dec Reg
  | Jnz ReadVal ReadVal
  deriving (Show)

instrP :: ReadP Instr
instrP =
  foldl1'
    (<++)
    [ binary "cpy" Cpy readValP regP
    , unary "inc" Inc regP
    , unary "dec" Dec regP
    , binary "jnz" Jnz readValP readValP
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

interpret :: forall s. V.Vector Instr -> Bool -> ST s Int
interpret instrs part2 = do
  regs <- VM.replicate 4 (0 :: Int)
  when part2 do
    VM.unsafeWrite regs 2 1
  let getVal = \case
        Left i -> pure i
        Right (Reg i) -> VM.unsafeRead regs i
  fix
    (\go pc ->
       if pc < 0 || pc >= V.length instrs
         then getVal $ Right (Reg 0)
         else case instrs V.! pc of
           Cpy x (Reg y) -> do
             v <- getVal x
             VM.unsafeWrite regs y v
             go (pc + 1)
           Inc (Reg x) -> do
             VM.unsafeModify regs (+ 1) x
             go (pc + 1)
           Dec (Reg x) -> do
             VM.unsafeModify regs (subtract 1) x
             go (pc + 1)
           Jnz x y -> do
             cond <- getVal x
             offset <- getVal y
             go (pc + if cond /= 0 then offset else 1))
    0

instance Solution Day12 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    instrs <- V.fromList . fmap (consumeOrDie instrP) . lines <$> getInputS
    answerShow $ runST (interpret instrs False)
    answerShow $ runST (interpret instrs True)
