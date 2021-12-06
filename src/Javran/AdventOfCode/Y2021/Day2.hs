{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Javran.AdventOfCode.Y2021.Day2
  (
  )
where

import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Maybe
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP

data Day2 deriving (Generic)

data Op
  = OpFwd Int
  | OpDown Int
  | OpUp Int

opP :: ReadP Op
opP = do
  c <-
    (OpFwd <$ string "forward")
      <++ (OpDown <$ string "down")
      <++ (OpUp <$ string "up")
  _ <- char ' '
  d <- decimal1P
  pure $ c d

type Pos = (Int, Int) -- hor, depth

interpret :: Op -> State Pos ()
interpret op = case op of
  OpFwd d -> modify (first (+ d))
  OpDown v -> modify (second (+ v))
  OpUp v -> modify (second (subtract v))

type Pos2 = (Pos, Int)

interpret2 :: Op -> State Pos2 ()
interpret2 op = case op of
  OpDown x -> modify (second (+ x))
  OpUp x -> modify (second (subtract x))
  OpFwd x -> do
    aim <- gets snd
    modify (first (bimap (+ x) (+ aim * x)))

instance Solution Day2 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    ops <- fmap (fromJust . consumeAllWithReadP opP) . lines <$> getInputS
    do
      let (horiz, dep) = execState (mapM_ interpret ops) (0, 0)
      answerShow (horiz * dep)
    do
      let ((horiz, dep), _aim) = execState (mapM_ interpret2 ops) ((0, 0), 0)
      answerShow (horiz * dep)
