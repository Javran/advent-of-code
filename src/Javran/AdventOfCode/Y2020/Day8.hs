{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2020.Day8
  (
  )
where

{- HLINT ignore "Unused LANGUAGE pragma" -}

import Control.Monad
import Control.Monad.State
import Data.Bifunctor
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import qualified Data.List.Split as LSplit
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP

data Day8

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
  solutionIndex _ = (2020, 8)
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
