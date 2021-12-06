{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2020.Day11
  (
  )
where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Monoid
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (many)

{-

  TODO: a potentially interesting idea: https://chrispenner.ca/posts/conways-game-of-life

  but the problem here is that we don't know sizes statically
  (unless we can figure out how to turn Int to type level Nat)

 -}

data Day11 deriving (Generic)

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

stepWorld :: (World Cell -> Coord -> Cell -> Cell) -> World Cell -> World Cell
stepWorld cellStepper w =
  V.imap
    (\r curRow ->
       V.imap
         (\c cur ->
            cellStepper w (r, c) cur)
         curRow)
    w

type WorldDim = (Int, Int)

neighborCoords :: WorldDim -> Coord -> [Coord]
neighborCoords (rows, cols) (r, c) = do
  r' <- [r -1 .. r + 1]
  guard $ r' >= 0 && r' < rows
  c' <- [c -1 .. c + 1]
  let v' = (r', c')
  guard $ c' >= 0 && c' < cols && (r, c) /= v'
  pure v'

stepCell :: WorldDim -> World Cell -> Coord -> Cell -> Cell
stepCell dims w coord cur = case cur of
  SeatEmpty | ns == 0 -> SeatOccupied
  SeatOccupied | ns >= 4 -> SeatEmpty
  _ -> cur
  where
    ns = countLength (== SeatOccupied) $ do
      (r', c') <- neighborCoords dims coord
      pure $ w V.! r' V.! c'

-- going one direction until hitting something
torpedo :: World Cell -> (Coord -> Maybe Coord) -> Coord -> Cell
torpedo w nextCoord coord@(r, c) =
  (w V.! r V.! c) <|> do
    c' <- nextCoord coord
    torpedo w nextCoord c'

torpedoNeighbors :: WorldDim -> World Cell -> Coord -> [Cell]
torpedoNeighbors (rows, cols) w coord = do
  dr <- [-1 .. 1]
  dc <- [-1 .. 1]
  guard $ (dr, dc) /= (0, 0)
  let nextCoord (r, c) = do
        let v'@(r', c') = (r + dr, c + dc) :: Coord
        guard $ r' >= 0 && r' < rows && c' >= 0 && c' < cols
        pure v'
  Just coord' <- [nextCoord coord]
  pure $ torpedo w nextCoord coord'

stepCell2 :: WorldDim -> World Cell -> Coord -> Cell -> Cell
stepCell2 dims w coord cur = case cur of
  SeatEmpty | ns == 0 -> SeatOccupied
  SeatOccupied | ns >= 5 -> SeatEmpty
  _ -> cur
  where
    ns = countLength (== SeatOccupied) $ torpedoNeighbors dims w coord

instance Solution Day11 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    rawLines <- lines <$> getInputS
    let initWorld :: World Cell
        initWorld =
          V.fromList $
            V.fromList . fromJust . consumeAllWithReadP (many cellP) <$> rawLines
        rows = V.length initWorld
        cols = V.length (V.head initWorld)
        dims = (rows, cols)

        _ppr w = do
          forM_ w $ \r ->
            putStrLn (V.toList $ fmap _pprCell r)
          putStrLn ""
    forM_ [stepCell, stepCell2] $ \cellStepper -> do
      let finalWorld = findFix (stepWorld (cellStepper dims)) initWorld
      answerShow $
        getSum @Int $
          (foldMap . foldMap)
            (\case
               SeatOccupied -> 1
               _ -> 0)
            finalWorld
