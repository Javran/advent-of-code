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

module Javran.AdventOfCode.Y2021.Day16
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Bits
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
import Data.Monoid
import Data.Ord
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Debug.Trace
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Numeric
import Text.ParserCombinators.ReadP hiding (count, many)

data Day16 deriving (Generic)

hexToBits :: Char -> [Int]
hexToBits ch = fmap (\pos -> if testBit v pos then 1 else 0) [3, 2, 1, 0]
  where
    v :: Int
    [(v, "")] = readHex [ch]

bitsToInt :: [Int] -> Int
bitsToInt = foldl (\a i -> a * 2 + i) 0

data Packet
  = PacketLiteral
      { pVersion :: Int
      , pType :: Int
      , pLiteral :: Int
      }
  | PacketOperator
      { pVersion :: Int
      , pType :: Int
      , pPayload :: [Packet]
      }
  deriving (Show)

packetVersionSum :: Packet -> Int
packetVersionSum = \case
  PacketLiteral {pVersion} -> pVersion
  PacketOperator {pVersion, pPayload} ->
    let f' = sum . fmap packetVersionSum
     in pVersion + f' pPayload

parsePacket :: State [Int] Packet
parsePacket = do
  pVersion <- bitsToInt <$> state (splitAt 3)
  pType <- bitsToInt <$> state (splitAt 3)
  case pType of
    4 ->
      fix
        (\loop acc -> do
           grp <- state (splitAt 5)
           case grp of
             0 : gs -> do
               let xs = acc <> gs
               pure $ PacketLiteral {pVersion, pType, pLiteral = bitsToInt xs}
             1 : gs -> loop (acc <> gs)
             _ -> unreachable)
        []
    _ -> do
      -- operator
      lt <- state (splitAt 1)
      let [lenTyp] = lt
      case lenTyp of
        0 -> do
          -- next 15 bit for bit len
          bitLen <- bitsToInt <$> state (splitAt 15)
          payload <- state (splitAt bitLen)
          let subP =
                unfoldr
                  (\left -> do
                     guard $ not (null left)
                     let (p, left') = runState parsePacket left
                     pure (p, left'))
                  payload

          pure PacketOperator {pVersion, pType, pPayload = subP}
        1 -> do
          packetCount <- bitsToInt <$> state (splitAt 11)
          ps <- replicateM packetCount parsePacket
          pure PacketOperator {pVersion, pType, pPayload = ps}
        _ -> unreachable

evalPacket = \case
  PacketLiteral {pLiteral} -> pLiteral
  PacketOperator {pType, pPayload} ->
    case pType of
      0 -> sum evaled
      1 -> product evaled
      2 -> minimum evaled
      3 -> maximum evaled
      5 ->
        let [l, r] = evaled
         in bool 0 1 (l > r)
      6 ->
        let [l, r] = evaled
         in bool 0 1 (l < r)
      7 ->
        let [l, r] = evaled
         in bool 0 1 (l == r)
      _ -> unreachable
    where
      evaled = fmap evalPacket pPayload

instance Solution Day16 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- concatMap hexToBits . head . lines <$> getInputS
    let (r, left) = runState parsePacket xs
    answerShow (packetVersionSum r)
    answerShow (evalPacket r)
