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

module Javran.AdventOfCode.Y2016.Day21
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Lens hiding (op)
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
import Javran.AdventOfCode.Misc
import Javran.AdventOfCode.Misc (rotateRightBy)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day21 deriving (Generic)

data Operation
  = SwapPos Int Int
  | SwapCh Char Char
  | RotStep (Either Int Int)
  | RotCh Char
  | Rev Int Int
  | Move Int Int
  deriving (Show)

operationP :: ReadP Operation
operationP =
  (string "swap " *> swapsP)
    <++ (string "rotate " *> rotatesP)
    <++ revP
    <++ moveP
  where
    positionP = string "position " *> decimal1P
    letterP = string "letter " *> nextCharP
    swapsP =
      (do
         x <- positionP
         _ <- string " with "
         y <- positionP
         pure $ SwapPos x y)
        <++ (do
               x <- letterP
               _ <- string " with "
               y <- letterP
               pure $ SwapCh x y)
    rotatesP =
      (do
         _ <- string "based on position of "
         x <- letterP
         pure $ RotCh x)
        <++ (do
               mk <- (Left <$ string "left") <++ (Right <$ string "right")
               _ <- char ' '
               x <- decimal1P
               _ <- string " step"
               _ <- void (char 's') <++ pure ()
               pure $ RotStep (mk x))
    revP = do
      _ <- string "reverse positions "
      xIn <- decimal1P
      _ <- string " through "
      yIn <- decimal1P
      let MinMax (x, y) = minMaxFromPair (xIn, yIn)
      pure $ Rev x y
    moveP = do
      _ <- string "move "
      x <- positionP
      _ <- string " to "
      y <- positionP
      pure $ Move x y

applyOp :: Int -> Operation -> [] Char -> [] Char
applyOp n = \case
  SwapPos i j -> \xs ->
    let a = xs !! i
        b = xs !! j
     in xs & ix i .~ b & ix j .~ a
  SwapCh a b -> \xs ->
    let Just i = findIndex (== a) xs
        Just j = findIndex (== b) xs
     in xs & ix i .~ b & ix j .~ a
  RotStep (Left i) -> rotateLeftBy n i
  RotStep (Right i) -> rotateRightBy n i
  RotCh x -> \xs ->
    let Just i = findIndex (== x) xs
        extra = if i >= 4 then 1 else 0
     in rotateRightBy n ((i + 1 + extra) `mod` n) xs
  Rev i j ->
    let sliceLen = j - i + 1
        f xs = reverse ys <> zs
          where
            (ys, zs) = splitAt sliceLen xs
     in rotateRightBy n i . f . rotateLeftBy n i
  Move i j -> \xs ->
    let x = xs !! i
     in fmap snd . insertBy (comparing fst) (j, x) . zip [0 ..] . delete x $ xs

instance Solution Day21 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerS} = do
    ops <- fmap (consumeOrDie operationP) . lines <$> getInputS
    let initSeq = "abcdefgh"
        applyAll z = foldl' (\cur op -> applyOp (length initSeq) op cur) z ops
        xs = applyAll initSeq
    answerS xs
    do
      -- search space isn't huge, meaning we can bruteforce this.
      let ps = permutations initSeq
          ans2 : _ = filter ((== "fbgdceah") . applyAll) ps
      answerS ans2
