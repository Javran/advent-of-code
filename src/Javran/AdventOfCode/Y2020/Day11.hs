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

import Control.Applicative
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
import Text.ParserCombinators.ReadP hiding (many)

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

findFix :: Eq a => (a -> a) -> a -> a
findFix f z = v
  where
    xs = iterate f z
    (_, v) : _ = dropWhile (uncurry (/=)) $ zip xs (tail xs)

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
        stepCell :: World Cell -> Coord -> Cell -> Cell
        stepCell w coord cur = case cur of
          SeatEmpty | ns == 0 -> SeatOccupied
          SeatOccupied | ns >= 4 -> SeatEmpty
          _ -> cur
          where
            ns = countLength (== SeatOccupied) $ do
              (r', c') <- neighborCoords coord
              pure $ w V.! r' V.! c'

        stepWorld :: World Cell -> World Cell
        stepWorld w =
          V.imap
            (\r curRow ->
               V.imap
                 (\c cur ->
                    stepCell w (r, c) cur)
                 curRow)
            w
        _ppr w = do
          forM_ w $ \r ->
            putStrLn (V.toList $ fmap _pprCell r)
          putStrLn ""
    let finalWorld = findFix stepWorld initWorld
    answerShow $
      getSum @Int $
        (foldMap . foldMap)
          (\case
             SeatOccupied -> 1
             _ -> 0)
          finalWorld
    let torpedo w coord@(r, c) nextCoord =
          (w V.! r V.! c) <|> do
            c' <- nextCoord coord
            torpedo w c' nextCoord
        torpedoNeighbors :: World Cell -> Coord -> [Cell]
        torpedoNeighbors w coord = do
          dr <- [-1 .. 1]
          dc <- [-1 .. 1]
          guard $ (dr, dc) /= (0, 0)
          let nextCoord (r, c) = do
                let v'@(r', c') = (r + dr, c + dc) :: Coord
                guard $ r' >= 0 && r' < rows && c' >= 0 && c' < cols
                pure v'
          Just coord' <- [nextCoord coord]
          pure $ torpedo w coord' nextCoord
        stepCell2 :: World Cell -> Coord -> Cell -> Cell
        stepCell2 w coord cur = case cur of
          SeatEmpty | ns == 0 -> SeatOccupied
          SeatOccupied | ns >= 5 -> SeatEmpty
          _ -> cur
          where
            ns = countLength (== SeatOccupied) $ torpedoNeighbors w coord
        stepWorld2 :: World Cell -> World Cell
        stepWorld2 w =
          V.imap
            (\r curRow ->
               V.imap
                 (\c cur ->
                    stepCell2 w (r, c) cur)
                 curRow)
            w
    let finalWorld2 = findFix stepWorld2 initWorld
    answerShow $
      getSum @Int $
        (foldMap . foldMap)
          (\case
             SeatOccupied -> 1
             _ -> 0)
          finalWorld2
