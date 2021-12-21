{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
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

module Javran.AdventOfCode.Y2019.Day22
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Bool
import Data.Char
import Data.Either
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Mod
import Data.Monoid
import Data.Ord
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Math.NumberTheory.Moduli.Class
import Text.ParserCombinators.ReadP hiding (count, many)

data Day22 deriving (Generic)

{-

  Note: all revertible under modulo p:

  - deal into new stack:

   x => - x, its revert is: x => - x (same)

  - cut N:

   This one is actually the tricky one:

   + find 0-th item i and N-th item j in current sequence

   + x => x + (j - i), this shifts the N-th item to 0-th location,
     and the rest of the items just follow.

   To revert this, simply do `cut -N`.

  - deal with increment N:

   x => x * N^-1, it's revert is x => x * N

   where N^-1 * N = 1

 -}

data ShufTech
  = StDealIntoNewStack
  | StCutCards Int
  | StDealWithIncrement Int
  deriving (Show)

shufTechP :: ReadP ShufTech
shufTechP =
  (StCutCards <$> (string "cut " *> readS_to_P (reads @Int)))
    <++ do
      _ <- string "deal "
      (StDealIntoNewStack <$ string "into new stack")
        <++ (StDealWithIncrement <$> (string "with increment " *> decimal1P))

revertShufTech :: SomeMod -> ShufTech -> SomeMod
revertShufTech x = \case
  StDealIntoNewStack -> x * (-1)
  StCutCards n -> x - fromIntegral n -- TODO: definitely wrong.
  StDealWithIncrement n -> x * fromIntegral n

applyShufTech :: SomeMod -> ShufTech -> SomeMod
applyShufTech x = \case
  StDealIntoNewStack -> x * (-1)
  StCutCards n -> x + fromIntegral n -- TODO: definitely wrong.
  StDealWithIncrement n -> case x of
    SomeMod x' -> SomeMod (x' * fromJust (invertMod (fromIntegral n `asTypeOf` x')))
    _ -> unreachable

instance Solution Day22 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (consumeOrDie shufTechP) . lines <$> getInputS
    let extract = \case
          SomeMod m -> getVal m
          _ -> unreachable
    print (length xs)
    print
      (fmap
         (\k ->
            extract $
              foldl
                applyShufTech
                (k `modulo` 10)
                [ StDealWithIncrement 7
                , StDealWithIncrement 9
                ])
         [0 .. 9])
