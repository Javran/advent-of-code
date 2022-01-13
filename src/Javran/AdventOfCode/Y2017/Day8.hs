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

module Javran.AdventOfCode.Y2017.Day8
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
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
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day8 deriving (Generic)

type Reg = String

type Instr = ((Reg, Int), (Reg, Int -> Bool))

instrP :: ReadP Instr
instrP = do
  let regP = munch1 isAlpha
      intP = readS_to_P (reads @Int)
  r0 <- regP <* char ' '
  incr <-
    (string "inc " *> intP)
      <++ (string "dec " *> (negate <$> intP))
  _ <- string " if "
  r1 <- regP
  _ <- char ' '
  op <-
    ((>=) <$ string ">=")
      <++ ((==) <$ string "==")
      <++ ((<=) <$ string "<=")
      <++ ((/=) <$ string "!=")
      <++ ((>) <$ string ">")
      <++ ((<) <$ string "<")
  _ <- char ' '
  n <- intP
  pure ((r0, incr), (r1, \val -> val `op` n))

interpret :: Instr -> M.Map String Int -> M.Map String Int
interpret ((mutReg, mutIncr), (condReg, predicate)) mem =
  if predicate condVal
    then
      M.alter
        (\case
           Nothing -> Just mutIncr
           Just v -> Just (v + mutIncr))
        mutReg
        mem
    else mem
  where
    condVal = fromMaybe 0 $ mem M.!? condReg

instance Solution Day8 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (consumeOrDie instrP) . lines <$> getInputS
    let m = execState (mapM (\x -> modify (interpret x)) xs) M.empty
    answerShow (maximum m)
