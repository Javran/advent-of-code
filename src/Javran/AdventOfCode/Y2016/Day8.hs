{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2016.Day8
  (
  )
where

import Control.Lens
import Control.Monad
import Data.List
import Javran.AdventOfCode.Misc (rotateRightBy)
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.TestExtra
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day8 deriving (Generic)

type Dims = (Int, Int)

data Operation
  = Rect Int Int
  | RotCol Int Int
  | RotRow Int Int
  deriving (Show)

operationP :: Dims -> ReadP Operation
operationP (colLim, rowLim) = rectP <++ rotateP
  where
    rectP = do
      a <- string "rect " *> decimal1P
      guard $ a < colLim
      b <- char 'x' *> decimal1P
      guard $ b < rowLim
      pure $ Rect a b
    rotateP = do
      _ <- string "rotate "
      (mk, ltN) <-
        ((RotCol, colLim) <$ string "column x=")
          <++ ((RotRow, rowLim) <$ string "row y=")
      a <- decimal1P
      guard $ a < ltN
      _ <- string " by "
      b <- decimal1P
      pure $ mk a b

type Screen = [[Bool]]

performOp :: Dims -> Operation -> Screen -> Screen
performOp (colLim, rowLim) = \case
  Rect w t -> \scr ->
    let (scrUp, scrDown) = splitAt t scr
        updRow = zipWith (||) (replicate w True <> repeat False)
     in fmap updRow scrUp <> scrDown
  RotCol c n ->
    let upd = rotateRightBy rowLim n
     in transpose . (& ix c %~ upd) . transpose
  RotRow r n ->
    let upd = rotateRightBy colLim n
     in (& ix r %~ upd)

instance Solution Day8 where
  solutionRun _ SolutionContext {getInputS, answerShow, answerS, terminal} = do
    (ex, rawInput) <- consumeExtra getInputS
    let dims@(colLim, rowLim) = singleLineExtra (50, 6) ex
        ops = fmap (consumeOrDie (operationP dims)) . lines $ rawInput
        initScr = replicate rowLim (replicate colLim False)
        scr = foldl' (flip (performOp dims)) initScr ops
        (ff, tt) = maybe (".", "#") (\_ -> ("  ", "██")) terminal
    answerShow $ countLength id (concat scr)
    forM_ scr \row ->
      answerS (concatMap (bool ff tt) row)
