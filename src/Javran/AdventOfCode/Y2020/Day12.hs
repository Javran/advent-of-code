{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Javran.AdventOfCode.Y2020.Day12
  (
  )
where

import Data.Bifunctor
import Data.Monoid
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP

data Day12 deriving (Generic)

-- coord = (x, y), N: +y, S: -y, W: -x, E: +x
type Coord = (Int, Int)

-- clockwisely
data Dir = N | E | S | W deriving (Enum, Show)

type Ship = (Coord, Dir)

data ShipInstr
  = SiDir Dir Int
  | SiTurn Bool {- True if turning right (clockwise) -} Int
  | SiForward Int

interpret :: ShipInstr -> Ship -> Ship
interpret si = case si of
  SiDir dir dist -> applyDir dir dist
  SiTurn right deg -> turnRight (if right then deg else - deg)
  SiForward d -> moveForward d

moveForward :: Int -> Ship -> Ship
moveForward d s@(_, h) = first modifyCoord s
  where
    modifyCoord = applyDirToCoord h d

applyDirToCoord :: Dir -> Int -> Coord -> Coord
applyDirToCoord dir dist = case dir of
  N -> second (+ dist)
  S -> second (subtract dist)
  W -> first (subtract dist)
  E -> first (+ dist)

applyDir :: Dir -> Int -> Ship -> Ship
applyDir dir dist (coord, h) = (coord', h)
  where
    (coord', _) = moveForward dist (coord, dir)

turnRight :: Int -> Ship -> Ship
turnRight deg (coord, h) = (coord, toEnum ((fromEnum h + dDir) `mod` 4))
  where
    (dDir, 0 {- panic, if not. -}) = deg `divMod` 90

actionP :: ReadP ShipInstr
actionP =
  foldr1
    (<++)
    [ do
        dir <-
          (N <$ char 'N')
            <++ (E <$ char 'E')
            <++ (S <$ char 'S')
            <++ (W <$ char 'W')
        dist <- decimal1P
        pure $ SiDir dir dist
    , do
        _ <- char 'L'
        deg <- decimal1P
        pure $ SiTurn False deg
    , do
        _ <- char 'R'
        deg <- decimal1P
        pure $ SiTurn True deg
    , do
        _ <- char 'F'
        d <- decimal1P
        pure $ SiForward d
    ]

type Ship2 = (Coord, Coord {- waypoint -})

rotateR :: Coord -> Coord
rotateR (x, y) = (y, - x)

rotateRight :: Int -> Coord -> Coord
rotateRight deg = appEndo (mconcat (replicate cnt (Endo rotateR)))
  where
    cnt = cntPre `mod` 4
    (cntPre, 0 {- panic, if not. -}) = deg `divMod` 90

interpret2 :: ShipInstr -> Ship2 -> Ship2
interpret2 si = case si of
  SiDir dir dist -> second (applyDirToCoord dir dist)
  SiTurn right deg -> second (rotateRight (if right then deg else - deg))
  SiForward t -> \((x, y), w@(dx, dy)) -> ((x + dx * t, y + dy * t), w)

instance Solution Day12 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    rawLines <- lines <$> getInputS
    let instrs = fmap (fromJust . consumeAllWithReadP actionP) rawLines
    do
      let action = mconcat $ reverse $ fmap (Endo . interpret) instrs
          ((x, y), _) = appEndo action ((0, 0), E)
      answerShow $ abs x + abs y
    do
      let action2 = mconcat $ reverse $ fmap (Endo . interpret2) instrs
          ((x, y), _) = appEndo action2 ((0, 0), (10, 1))
      answerShow $ abs x + abs y
