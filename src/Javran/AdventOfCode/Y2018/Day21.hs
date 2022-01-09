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

module Javran.AdventOfCode.Y2018.Day21
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
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
import Text.ParserCombinators.ReadP hiding (count, many, get)
import Javran.AdventOfCode.Y2018.Day19

data Day21 deriving (Generic)

{-
  My login input:

  IP is bound to R3
   0    r1 = 123
   1    r1 = r1 & 456
   2    r1 = r1 == 72
   3    jmp (r1 + 3)+1
   4    jmp (0)+1
   5    r1 = 0
   6    r2 = r1 | 65536
   7    r1 = 10605201
   8    r5 = r2 & 255
   9    r1 = r1 + r5
  10    r1 = r1 & 16777215
  11    r1 = r1 * 65899
  12    r1 = r1 & 16777215
  13    r5 = 256 >= r2
  14    jmp (r5 + 14)+1
  15    jmp (15 + 1)+1
  16    jmp (27)+1
  17    r5 = 0
  18    r4 = r5 + 1
  19    r4 = r4 * 256
  20    r4 = r4 >= r2
  21    jmp (r4 + 21)+1
  22    jmp (22 + 1)+1
  23    jmp (25)+1
  24    r5 = r5 + 1
  25    jmp (17)+1
  26    r2 = r5
  27    jmp (7)+1
  28    r5 = r1 == r0
  29    jmp (r5 + 29)+1
  30    jmp (5)+1

 -}

instance Solution Day21 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- consumeOrDie programP <$> getInputS
    pprProgram xs
