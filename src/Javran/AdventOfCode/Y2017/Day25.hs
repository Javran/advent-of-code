{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Javran.AdventOfCode.Y2017.Day25
  (
  )
where

import Control.Monad
import Control.Monad.State.Strict
import qualified Data.Array as Arr
import Data.Char
import Data.Semigroup
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day25 deriving (Generic)

newtype MState = MState Int
  deriving stock (Eq, Ord)
  deriving (Enum) via Int

instance Show MState where
  show (MState v) = "MState " <> [chr (ord 'A' + v)]

data Move = ML | MR deriving (Show)

type WriteMoveTransit = (Bool, Move, MState)

type Rules = Arr.Array (Int, Int) WriteMoveTransit

type Input = ((MState, Int), Rules)

type TapeMemory = (Bool, ([Bool], [Bool]))

type TapeMachine = (MState, TapeMemory)

inputP :: ReadP Input
inputP = do
  let nl = void (char '\n')
      dotNl = void (string ".\n")
      valP = (False <$ char '0') <++ (True <$ char '1')
      moveP = (ML <$ string "left") <++ (MR <$ string "right")
      stateP =
        string "state " *> do
          ch <- satisfy isAsciiUpper
          pure $ MState $ ord ch - ord 'A'
      stateDescP = do
        s0 <- between (string "In ") (string ":\n") stateP
        ~[c0, c1] <- forM ["0", "1"] \condV -> do
          _ <- string $ "  If the current value is " <> condV <> ":\n"
          w <- between (string "    - Write the value ") dotNl valP
          m <- between (string "    - Move one slot to the ") dotNl moveP
          t <- between (string "    - Continue with ") dotNl stateP
          pure (w, m, t)
        pure (s0, (c0, c1))
  v0 <-
    (,)
      <$> between (string "Begin in ") (string ".\n") stateP
        <*> between (string "Perform a diagnostic checksum after ") (string " steps.\n") decimal1P
  parsedRules <- many1 (nl *> stateDescP)
  let Just (Max (MState maxMs)) = foldMap (Just . Max . fst) parsedRules
      rules = Arr.array ((0, 0), (maxMs, 1)) do
        (MState lhs, (c0, c1)) <- parsedRules
        [((lhs, 0), c0), ((lhs, 1), c1)]
  pure (v0, rules)

type Sim = State TapeMachine

moveLeft, moveRight :: TapeMemory -> TapeMemory
moveLeft (foc, (ls, rs)) = case ls of
  [] -> (False, (ls, rs'))
  (l : ls') -> (l, (ls', rs'))
  where
    rs' = foc : rs
moveRight (foc, (ls, rs)) = case rs of
  [] -> (False, (ls', rs))
  (r : rs') -> (r, (ls', rs'))
  where
    ls' = foc : ls

step :: Rules -> Sim ()
step rules = do
  (w, m, t) <- gets (\(st, (foc, _)) -> rules Arr.! (fromEnum st, fromEnum foc))
  modify
    (bimap
       (\_ -> t)
       ((case m of
           ML -> moveLeft
           MR -> moveRight)
          . first (\_ -> w)))

instance Solution Day25 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    ((s0, cnt), rules) <- consumeOrDie inputP <$> getInputS
    let (_, (foc, (ls, rs))) =
          execState (replicateM_ cnt (step rules)) (s0, (False, ([], [])))
    answerShow $ countLength id ls + countLength id (foc:rs)
