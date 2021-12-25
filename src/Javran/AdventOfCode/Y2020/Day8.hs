{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Javran.AdventOfCode.Y2020.Day8
  (
  )
where

import Control.Monad.State
import qualified Data.IntSet as IS
import qualified Data.Vector as V
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP

data Day8 deriving (Generic)

data Instr
  = Nop Int
  | Acc Int
  | Jmp Int
  | Term
  deriving (Show)

type Machine =
  ( Int {- accumulator -}
  , IS.IntSet {- reached program positions -}
  )

instrP :: ReadP Instr
instrP = do
  mk <-
    (Nop <$ string "nop")
      <++ (Acc <$ string "acc")
      <++ (Jmp <$ string "jmp")
  _ <- char ' '
  numMod <- (id <$ char '+') <++ (negate <$ char '-')
  mk . numMod <$> decimal1P

instance Solution Day8 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    rawInstrs <- lines <$> getInputS
    let originalInstrs =
          V.fromList $ fmap (fromJust . consumeAllWithReadP instrP) rawInstrs
        interpretProgram :: V.Vector Instr -> Int -> State Machine Bool
        interpretProgram instrs = fix $ \interpret pos -> do
          reached <- gets (IS.member pos . snd)
          if reached
            then pure False
            else do
              modify (second (IS.insert pos))
              case instrs V.! pos of
                Nop _ -> interpret (pos + 1)
                Acc d -> do
                  modify (first (+ d))
                  interpret (pos + 1)
                Jmp d -> interpret (pos + d)
                Term -> pure True
    answerShow (fst $ execState (interpretProgram originalInstrs 0) (0, IS.empty))
    let instrs2 = originalInstrs <> V.singleton Term
        patchAtPos i = case instrs2 V.! i of
          Nop x -> [instrs2 V.// [(i, Jmp x)]]
          Jmp x -> [instrs2 V.// [(i, Nop x)]]
          _ -> []
    answerShow $
      head $ do
        patchPos <- [0 .. V.length originalInstrs - 1]
        instrs2' <- patchAtPos patchPos
        (True, (acc, _)) <- [runState (interpretProgram instrs2' 0) (0, IS.empty)]
        pure acc
