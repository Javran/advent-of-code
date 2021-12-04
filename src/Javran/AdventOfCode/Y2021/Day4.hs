{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2021.Day4
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Data.Bool
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (many)

data Day4

type Board = [[Int]]

bingoSet :: Board -> [IS.IntSet]
bingoSet bd = fmap IS.fromList bd <> fmap IS.fromList (transpose bd)

playGame callSeq bds allBingoSetsPre pairedAllCallSeqs won = case tmp of
  (finalCallSeq , justCalled): keepGoing ->
      let
        (which, _) : _ = filter (\(_, bSets) -> any (\b -> IS.null (b `IS.difference` finalCallSeq)) bSets) allBingoSets
        unmarkedNums = (IS.fromList $ concat $ bds !! which) `IS.difference` finalCallSeq
     in Just ((which, sum $ IS.toList unmarkedNums, justCalled), (keepGoing, IS.insert which won))
  [] -> Nothing
  where
    (_, tmp) = span (noBingo . fst) $ pairedAllCallSeqs
    allBingoSets = filter (\(w, _) -> IS.notMember w won) allBingoSetsPre
    noBingo callSet = all (\(w, bSets) -> all (\b -> not $ IS.null (b `IS.difference` callSet)) bSets) allBingoSets

instance Solution Day4 where
  solutionIndex _ = (2021, 4)
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    [callSeqRaw] : bdsRaw <- splitOn [""] . lines <$> getInputS
    let callSeq :: [Int]
        callSeq = fmap read . splitOn "," $ callSeqRaw
    let bds :: [] [[Int]]
        bds = (fmap . fmap) (fmap read . words) bdsRaw
        allBingoSets = zip [0 :: Int ..] (fmap bingoSet bds)
        allCallSeqs = tail $ fmap IS.fromList $ inits callSeq
        pairedAllCallSeqs = zip allCallSeqs callSeq
        Just ((_ , unmarkedSum , justCalled), _)  = playGame callSeq bds allBingoSets pairedAllCallSeqs IS.empty
    answerShow (justCalled *  unmarkedSum)
    let (_, u, v) = last $ unfoldr (\(st, won) -> playGame callSeq bds allBingoSets pairedAllCallSeqs won) (pairedAllCallSeqs, IS.empty)
    answerShow (u * v)
