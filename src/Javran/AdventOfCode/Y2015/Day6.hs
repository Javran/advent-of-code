{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Javran.AdventOfCode.Y2015.Day6
  (
  )
where

import Control.Monad
import Control.Monad.ST
import qualified Data.Array.Base as Arr
import qualified Data.Array.Base as MArr
import Javran.AdventOfCode.GridSystem.RowThenCol.Uldr
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day6 deriving (Generic)

data Op = Turn Bool | Toggle deriving (Show)

type Instr = (Op, (Coord, Coord))

instrP :: ReadP Instr
instrP = do
  let coordP = (,) <$> decimal1P <* char ',' <*> decimal1P
  op <-
    (Toggle <$ string "toggle ")
      <++ (string "turn "
             *> (Turn
                   <$> ((False <$ string "off ")
                          <++ (True <$ string "on "))))
  a <- coordP
  strP " through "
  b <- coordP
  guard $ a <= b
  pure (op, (a, b))

simulate
  :: forall s a ans.
  (Arr.MArray (Arr.STUArray s) a (ST s))
  => [Instr]
  -> a
  -> (Op -> a -> a)
  -> ([a] -> ans)
  -> ST s ans
simulate instrs z opToFn toAnswer = do
  grid <- MArr.newArray @(Arr.STUArray s) ((0, 0), (999, 999)) z
  forM_ instrs \(op, ((rLo, cLo), (rHi, cHi))) -> do
    let f = opToFn op
    forM_ [(r, c) | r <- [rLo .. rHi], c <- [cLo .. cHi]] \coord -> do
      v <- MArr.readArray grid coord
      MArr.writeArray grid coord (f v)

  toAnswer <$> MArr.getElems grid

instance Solution Day6 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    instrs <- fmap (consumeOrDie instrP) . lines <$> getInputS
    answerShow $
      runST $
        simulate
          instrs
          False
          (\case
             Turn v -> const v
             Toggle -> not)
          (countLength id)
    answerShow $
      runST $
        simulate
          instrs
          (0 :: Int)
          (\case
             Turn True -> (+ 1)
             Turn False -> \v -> max 0 (v -1)
             Toggle -> (+ 2))
          sum
