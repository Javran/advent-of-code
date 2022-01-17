{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2016.Day1
  (
  )
where

import Control.Monad
import qualified Data.Map.Strict as M
import Control.Monad.Writer.Lazy
import qualified Data.DList as DL
import Javran.AdventOfCode.GridSystem.RowThenCol.Nwse
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.TestExtra
import Text.ParserCombinators.ReadP hiding (count, get, many)
import Javran.AdventOfCode.Fixpoint

data Day1 deriving (Generic)

type Instr = (Dir -> Dir, Int)

instrP :: ReadP Instr
instrP = do
  turn <- (turnLeft <$ char 'L') <++ (turnRight <$ char 'R')
  n <- decimal1P
  pure (turn, n)

type LocDir = (Coord, Dir)

type Traced = Writer (DL.DList Coord)

applyDir' :: Dir -> Coord -> Traced Coord
applyDir' d coord = do
  let d' = applyDir d coord
  d' <$ tell (DL.singleton d')

applyInstr :: Instr -> LocDir -> Traced LocDir
applyInstr (turn, n) (loc0, d) = do
  let d' = turn d
  loc' <- foldM (\loc () -> applyDir' d' loc) loc0 (replicate n ())
  pure (loc', d')

instance Solution Day1 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (extraOps, rawInput) <- consumeExtra getInputS
    let instrs = consumeOrDie (instrP `sepBy` string ", " <* char '\n') rawInput
        (runPart1, runPart2) = shouldRun extraOps
        dist = manhattan (0, 0)
        ((loc, _), path) = runWriter (foldM (\ld instr -> applyInstr instr ld) ((0, 0), N) instrs)
    when runPart1 do
      answerShow $ dist loc
    when runPart2 do
      let (_, (loc', _)) = findFix M.empty (zip [0..] $ DL.toList path)
      answerShow $ dist loc'
