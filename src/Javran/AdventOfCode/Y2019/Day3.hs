{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2019.Day3
  (
  )
where

import Control.Monad
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Javran.AdventOfCode.GridSystem.RowThenCol.Uldr
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day3 deriving (Generic)

type Instr = (Dir, Int)

instrP :: ReadP Instr
instrP = do
  d <-
    (U <$ char 'U')
      <++ (D <$ char 'D')
      <++ (L <$ char 'L')
      <++ (R <$ char 'R')
  n <- decimal1P
  pure (d, n)

applyInstr :: Instr -> State ((Coord, Int), M.Map Coord Int) ()
applyInstr (d, n) = replicateM_ n do
  ((coord, cnt), acc) <- get
  let coord' = applyDir d coord
      cnt' = succ cnt
  put
    ( (coord', cnt')
    , M.alter
        (\case
           Nothing -> Just cnt'
           v@(Just _) -> v)
        coord'
        acc
    )

instance Solution Day3 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    [xs, ys] <- fmap (consumeOrDie (instrP `sepBy` char ',')) . lines <$> getInputS
    let initSt = (((0, 0), 0), M.empty)
        (_, mX) =
          execState (mapM_ applyInstr xs) initSt
        (_, mY) =
          execState (mapM_ applyInstr ys) initSt
        common = S.toList $ (S.intersection `on` M.keysSet) mX mY
    answerShow $
      minimum $ fmap (uncurry ((+) `on` abs)) common
    answerShow $
      minimum $ fmap (\coord -> (mX M.! coord) + (mY M.! coord)) common
