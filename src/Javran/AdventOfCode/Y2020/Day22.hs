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
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2020.Day22
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Bool
import qualified Data.ByteString as BS
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Debug.Trace
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (get, many)

data Day22

type GameState = ([Int], [Int])

{-
  there's a very limited amount of cards
  that their values comfortably fits within Word8.
  we can take advantage of this fact and use bytestring
  to check reoccuring game states in a more efficient manner.
 -}
encodeGameState :: GameState -> BS.ByteString
encodeGameState (xs, ys) =
  BS.pack (fmap fromIntegral $ xs <> [0xFF] <> ys)

stepGame :: GameState -> Maybe GameState
stepGame gs = case gs of
  ([], _) -> Nothing
  (_, []) -> Nothing
  (x : xs, y : ys) -> case compare x y of
    EQ -> undefined
    LT -> Just (xs, ys <> [y, x])
    GT -> Just (xs <> [x, y], ys)

playGame2 :: GameState -> State (S.Set BS.ByteString) (Bool, GameState)
playGame2 gs = do
  r <- stepGame2 gs
  case r of
    Left winner -> pure (winner, gs)
    Right gs' -> playGame2 gs'

{-
  winner is indicated by Bool, False for player 1 and True for player 2.
 -}
stepGame2 :: GameState -> State (S.Set BS.ByteString) (Either Bool GameState)
stepGame2 gs = do
  let encodedGs = encodeGameState gs
  occurred <- gets (S.member encodedGs)
  modify (S.insert encodedGs)
  if occurred
    then pure (Left False) -- instant win of player 1
    else case gs of
      ([], _) -> pure (Left True)
      (_, []) -> pure (Left False)
      (x : xs, y : ys) -> do
        if length xs >= x && length ys >= y
          then do
            let (subGameResult, _) = evalState (playGame2 (take x xs, take y ys)) S.empty
            if subGameResult
              then -- True, meaning player 2 won
                pure $ Right (xs, ys <> [y, x])
              else -- False, meaning player 1 won
                pure $ Right (xs <> [x, y], ys)
          else case compare x y of
            EQ -> undefined
            LT -> pure $ Right (xs, ys <> [y, x])
            GT -> pure $ Right (xs <> [x, y], ys)

instance Solution Day22 where
  solutionIndex _ = (2020, 22)
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    ["Player 1:" : xsRaw, "Player 2:" : ysRaw] <- splitOn [""] . lines <$> getInputS
    let xs, ys :: [Int]
        xs = fmap read xsRaw
        ys = fmap read ysRaw
        result =
          last $
            unfoldr
              (\s -> do
                 s' <- stepGame s
                 pure (s', s'))
              (xs, ys)
        winnerStack = ls <> rs
          where
            (ls, rs) = result
    answerShow $ sum $ zipWith (*) [1 ..] (reverse winnerStack)
    let (_w, st) = evalState (playGame2 (xs, ys)) S.empty
        winnerStack2 = fst st <> snd st
    answerShow $ sum $ zipWith (*) [1 ..] (reverse winnerStack2)
