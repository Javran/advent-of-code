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
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Math.NumberTheory.Moduli.Class
import Text.ParserCombinators.ReadP hiding (count, many)

data Day22 deriving (Generic)

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

applyShufTech :: Int -> ShufTech -> VU.Vector Int -> VU.Vector Int
applyShufTech m st xs = case st of
  StDealIntoNewStack -> VU.reverse xs
  StCutCards n -> let (ys, zs) = VU.splitAt (n `mod` m) xs in zs <> ys
  StDealWithIncrement n ->
    VU.create do
      v' <- VUM.new (VU.length xs)
      let n' = n `mod` m
      forM_ [0 .. VU.length xs -1] $ \i -> do
        let j = (i * n') `mod` m
        VUM.unsafeWrite v' j (xs VU.! i)
      pure v'

instance Solution Day22 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow, answerS} = do
    (extraOps, rawInput) <- consumeExtraLeadingLines <$> getInputS
    let xs = fmap (consumeOrDie shufTechP) . lines $ rawInput
    case extraOps of
      Nothing -> do
        let result =
              foldl
                (flip (applyShufTech 10007))
                (VU.fromListN 10007 [0 ..])
                xs
            (ans :: Int, _) : _ =
              filter ((== 2019) . snd) $ zip [0 ..] (VU.toList result)
        answerShow ans
      Just extra -> do
        let p = read @Int (head extra)
        mapM_ (answerS . unwords . fmap show . VU.toList) $
          tail $
            scanl
              (flip (applyShufTech p))
              (VU.fromListN p [0 ..])
              xs