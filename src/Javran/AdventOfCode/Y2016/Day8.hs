{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2016.Day8
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Char
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2017.Day6 (rotateRightBy)
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day8 deriving (Generic)

data Operation
  = Rect Int Int
  | RotCol Int Int
  | RotRow Int Int
  deriving (Show)

colLim, rowLim :: Int
(colLim, rowLim) = (50, 6)

operationP :: ReadP Operation
operationP = rectP <++ rotateP
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

performOp :: Operation -> Screen -> Screen
performOp = \case
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

pprScreen :: Screen -> IO ()
pprScreen = mapM_ \row -> do
  putStrLn (fmap (bool '.' '#') row)

instance Solution Day8 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    ops <- fmap (consumeOrDie operationP) . lines <$> getInputS
    let initScr = replicate rowLim (replicate colLim False)
        scr = foldl' (flip performOp) initScr ops
    pprScreen scr
    answerShow $ countLength id (concat scr)
