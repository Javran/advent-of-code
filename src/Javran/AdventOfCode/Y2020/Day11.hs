{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2020.Day11
  (
  )
where

{- HLINT ignore "Unused LANGUAGE pragma" -}

import Control.Monad
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import qualified Data.List.Split as LSplit
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Proxy
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP

{-

  TODO: a potentially interesting idea: https://chrispenner.ca/posts/conways-game-of-life

  but the problem here is that we don't know sizes statically
  (unless we can figure out how to turn Int to type level Nat)

 -}

data Day11

type Cell = Maybe Bool

pattern Floor :: Cell
pattern Floor = Nothing

pattern SeatEmpty :: Cell
pattern SeatEmpty = Just False

pattern SeatOccupied :: Cell
pattern SeatOccupied = Just True

{-# COMPLETE Floor, SeatEmpty, SeatOccupied #-}

type World a = V.Vector (V.Vector a) -- row-major

type Coord = (Int, Int)

cellP :: ReadP Cell
cellP =
  (Floor <$ char '.')
    <++ (SeatEmpty <$ char 'L')
    <++ (SeatOccupied <$ char '#')

_pprCell :: Cell -> Char
_pprCell = \case
  Floor -> '.'
  SeatEmpty -> 'L'
  SeatOccupied -> '#'

instance Solution Day11 where
  solutionIndex _ = (2020, 11)
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    rawLines <- lines <$> getInputS
    let initWorld :: World Cell
        initWorld =
          V.fromList $
            V.fromList . fromJust . consumeAllWithReadP (many cellP) <$> rawLines
        rows = V.length initWorld
        cols = V.length (V.head initWorld)
        neighborCoords :: Coord -> [Coord]
        neighborCoords (r, c) = do
          r' <- [r -1 .. r + 1]
          guard $ r' >= 0 && r' < rows
          c' <- [c -1 .. c + 1]
          let v' = (r', c')
          guard $ c' >= 0 && c' < cols && (r, c) /= v'
          pure v'
        countOccupiedNeighborSeats :: World Cell -> Coord -> Int
        countOccupiedNeighborSeats world coord = countLength (== SeatOccupied) $ do
          (r', c') <- neighborCoords coord
          pure $ world V.! r' V.! c'
        stepWorld :: World Cell -> World Cell
        stepWorld w =
          V.imap
            (\r curRow ->
               V.imap
                 (\c cur ->
                    let ns = countOccupiedNeighborSeats w (r, c)
                     in case cur of
                          SeatEmpty | ns == 0 -> SeatOccupied
                          SeatOccupied | ns >= 4 -> SeatEmpty
                          _ -> cur)
                 curRow)
            w
        _ppr w = do
          forM_ w $ \r ->
            putStrLn (V.toList $ fmap _pprCell r)
          putStrLn ""
    let (_, finalWorld) : _ = dropWhile (uncurry (/=)) $ zip progression (tail progression)
          where
            progression :: [World Cell]
            progression = iterate stepWorld initWorld
    answerShow $
      getSum @Int $
        (foldMap . foldMap)
          (\case
             SeatOccupied -> 1
             _ -> 0)
          finalWorld
