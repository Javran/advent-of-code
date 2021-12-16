{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Javran.AdventOfCode.Y2021.Day16
  (
  )
where

import Control.Monad
import Control.Monad.State.Strict
import Data.Bits
import Data.Bool
import qualified Data.DList as DL
import Data.Functor.Base (TreeF (..))
import Data.Functor.Foldable
import Data.List
import Data.Tree
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Numeric

data Day16 deriving (Generic)

hexToBits :: Char -> [Int]
hexToBits ch = (\pos -> bool 0 1 (testBit v pos)) <$> [3, 2, 1, 0]
  where
    v :: Int
    [(v, "")] = readHex [ch]

bitsToInt :: [Int] -> Int
bitsToInt = foldl' (\a i -> a * 2 + i) 0

type Packet = Tree PacketData

data PacketData
  = PacketLiteral
      { pVersion :: Int
      , pType :: Int
      , pLiteral :: Int
      }
  | PacketOperator
      { pVersion :: Int
      , pType :: Int
      }
  deriving (Show)

parsePacket :: State [Int] Packet
parsePacket = do
  pVersion <- consumeInt 3
  pType <- consumeInt 3
  case pType of
    4 ->
      -- literal
      fix
        (\loop acc -> do
           grp <- consume 5
           case grp of
             0 : gs ->
               pure $
                 Node
                   (PacketLiteral
                      { pVersion
                      , pType
                      , pLiteral =
                          bitsToInt $
                            DL.toList (acc <> DL.fromList gs)
                      })
                   []
             1 : gs -> loop (acc <> DL.fromList gs)
             _ -> unreachable)
        DL.empty
    _ -> do
      -- operator
      lenTyp <- consume 1
      case lenTyp of
        [0] -> do
          -- next 15 bit for bit len
          bitLen <- consumeInt 15
          payload <- consume bitLen
          let subP =
                unfoldr
                  (\leftover -> do
                     guard $ not (null leftover)
                     pure $ runState parsePacket leftover)
                  payload
          pure $ Node PacketOperator {pVersion, pType} subP
        [1] -> do
          packetCount <- consumeInt 11
          ps <- replicateM packetCount parsePacket
          pure $ Node PacketOperator {pVersion, pType} ps
        _ -> unreachable
  where
    consume n = state (splitAt n)
    consumeInt n = bitsToInt <$> consume n

packetVersionSum :: Packet -> Int
packetVersionSum = cata \(NodeF d rs) -> pVersion d + sum rs

evalPacket :: Packet -> Int
evalPacket = cata \(NodeF d rs) ->
  case d of
    PacketLiteral {pLiteral} -> pLiteral
    PacketOperator {pType} -> ($ rs) $
      case pType of
        0 -> sum
        1 -> product
        2 -> minimum
        3 -> maximum
        5 -> binOp (>)
        6 -> binOp (<)
        7 -> binOp (==)
        _ -> unreachable
      where
        binOp (<~>) [l, r] = bool 0 1 (l <~> r)
        binOp _ _ = unreachable

instance Solution Day16 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- concatMap hexToBits . head . lines <$> getInputS
    let r = evalState parsePacket xs
    answerShow (packetVersionSum r)
    answerShow (evalPacket r)
