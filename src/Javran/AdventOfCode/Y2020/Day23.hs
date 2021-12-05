{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2020.Day23
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Bifunctor
import Data.Bool
import Data.Char
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.STRef
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Debug.Trace
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (many)

data Day23

chInt :: Char -> Int
chInt v = ord v - ord '0'

intCh :: Int -> Char
intCh v = chr (v + ord '0')

{-
  head is always the current cup.
 -}
type CupState = [Int]

step :: CupState -> CupState
step = \case
  (foc : pre)
    | (xs, ys) <- splitAt 3 pre ->
      let downs = cycle [9, 8 .. 1]
          dest : _ = filter (`elem` ys) $ tail (dropWhile (/= foc) downs)
          (us, _dest : vs) = span (/= dest) ys
       in us <> (dest : xs) <> vs <> [foc]
  _ -> unreachable

toResult :: CupState -> String
toResult xs = fmap intCh $ take 8 $ tail $ dropWhile (/= 1) $ cycle xs

data Cup ref = Cup
  { _cupLabel :: Int
  , _cupNext :: ref (Cup ref)
  }

mixCups :: [Int] -> Int -> Int -> [Int]
mixCups xs len opCount = runST simulate
  where
    simulate :: forall s cup. cup ~ Cup (STRef s) => ST s [Int]
    simulate = do
      -- let vec[i] represent cup of label i+1.
      cups <- VM.unsafeNew len
      forM_ [1 .. len] $ \lbl -> do
        let i = lbl - 1
        r <- newSTRef (error $ "label " <> show lbl)
        VM.write cups i (Cup lbl r)
      let labels = xs <> [10 .. len]
          getCup lbl = VM.read cups (lbl -1)
      forM_ ((last labels, head labels) : zip labels (tail labels)) $ \(lblFrom, lblTo) -> do
        (Cup _ r) <- getCup lblFrom
        nTo <- getCup lblTo
        writeSTRef r nTo
      let performMove :: cup -> ST s cup
          performMove (Cup focLbl focRef) = do
            c1@(Cup lbl1 r1) <- readSTRef focRef
            c2@(Cup lbl2 r2) <- readSTRef r1
            c3@(Cup lbl3 r3) <- readSTRef r2
            cNext <- readSTRef r3
            -- removes c1,c2,c3
            writeSTRef focRef cNext
            let destCupLbl : _ =
                  filter (`notElem` [lbl1, lbl2, lbl3]) $
                    tail $ iterate nextDest focLbl
                  where
                    nextDest i = if i == 1 then len else i -1
            destCup@(Cup _ rDest) <- getCup destCupLbl
            -- insert
            afterDest <- readSTRef rDest
            writeSTRef rDest c1
            writeSTRef r3 afterDest
            pure cNext
      do
        hd <- getCup (head xs)
        fix
          (\loop cup cnt ->
             if cnt == 0
               then pure ()
               else do
                 cup' <- performMove cup
                 loop cup' (cnt - 1 :: Int))
          hd
          opCount
      do
        (Cup _ r0) <- getCup 1
        (Cup lbl1 r1) <- readSTRef r0
        (Cup lbl2 _) <- readSTRef r1
        pure [lbl1, lbl2]

instance Solution Day23 where
  solutionIndex _ = (2020, 23)
  solutionRun _ SolutionContext {getInputS, answerShow, answerS} = do
    (extraOpts, rawInput) <- consumeExtraLeadingLines <$> getInputS
    let xs = fmap chInt . head . lines $ rawInput
    -- looks like a safe assumption, make sure of it.
    True <- pure (sort xs == [1 .. 9])
    let final = iterate step xs !! 100
    answerS (toResult final)
    let (len, opCount) = case extraOpts of
          Nothing -> (1000000, 10000000)
          Just [rawExtra] -> read rawExtra
          _ -> errInvalid
    answerShow $ product $ mixCups xs len opCount
