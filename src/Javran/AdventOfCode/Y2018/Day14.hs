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

module Javran.AdventOfCode.Y2018.Day14
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Control.Monad.Writer.Lazy
import Data.Char
import qualified Data.DList as DL
import Data.Foldable
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day14 deriving (Generic)

type RecipeState = ((Int, Int), Seq.Seq Int)

step :: RecipeState -> (RecipeState, DL.DList Int)
step ((elf0, elf1), xs) =
  (((norm $ elf0 + v0 + 1, norm $ elf1 + v1 + 1), xs'), DL.fromList extra)
  where
    norm = (`rem` newL)
    v0 = Seq.index xs elf0
    v1 = Seq.index xs elf1
    extra =
      -- TODO: can't use intToDigits, as it'll generate [] instead of expected [0]
      if v0 + v1 > 9 then [1, v0 + v1 -10] else [v0 + v1]
    xs' = xs <> Seq.fromList extra
    newL = Seq.length xs'

recipes :: [Int]
recipes = 3 : 7 : DL.toList xs
  where
    xs = execWriter (gen initSt)
    gen s = do
      s' <- writer (step s)
      gen s'
    initSt = ((0, 1), Seq.fromList [3, 7])

instance Solution Day14 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerS, answerShow} = do
    rawN <- head . lines <$> getInputS
    do
      let n = read @Int rawN
      answerS (fmap (\v -> chr (v + ord '0')) $ take 10 $ toList $ drop n recipes)
    do
      let nSeq = fmap (read @Int . (: [])) rawN
          (ans, _) : _ =
            dropWhile ((nSeq /=) . snd) $
              zip [0 :: Int ..] (divvy (length nSeq) 1 recipes)
      answerShow ans
