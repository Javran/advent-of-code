{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Javran.AdventOfCode.Y2017.Day19
  ( Coord
  , Dir (..)
  , turnLeft
  , turnRight
  , applyDir
  )
where

import Control.Monad
import Control.Monad.Loops
import Control.Monad.RWS.CPS hiding (First)
import Data.Char
import qualified Data.DList as DL
import qualified Data.Map.Strict as M
import Data.Semigroup
import Javran.AdventOfCode.GridSystem.RowThenCol.Uldr
import Javran.AdventOfCode.Prelude

data Day19 deriving (Generic)

data Point
  = Normal
  | Letter Char
  | Crossing
  deriving (Show)

data MapInfo = MapInfo
  { miDiagram :: M.Map Coord Point
  , miStart :: (Coord, Dir)
  }
  deriving (Show)

parseFromRaw :: [String] -> MapInfo
parseFromRaw xs = MapInfo {miStart, miDiagram = M.fromList ps}
  where
    (Just (First miStart), ps) = mconcat do
      (r, rs) <- zip [0 ..] xs
      (c, x) <- zip [0 ..] rs
      let coord = (r, c)
          s = First (coord, D) <$ guard (r == 0 && x == '|')
      pure
        if
            | x == ' ' -> mempty
            | x `elem` "-|" -> (s, [(coord, Normal)])
            | x == '+' -> (s, [(coord, Crossing)])
            | isAlpha x -> (s, [(coord, Letter x)])
            | otherwise -> error $ "Cannot recognize: " <> show x

-- state is (current location, dir intended to go next)
type Sim = RWS MapInfo (DL.DList Char, Sum Int) (Coord, Dir)

newStep :: Sim ()
newStep = tell (mempty, 1)

step :: Sim (Maybe ())
step = do
  mapQuery <- asks ((M.!?) . miDiagram)
  next <- gets (\(cur, d) -> applyDir d cur)
  case mapQuery next of
    Just pt -> do
      newStep
      Nothing <$ case pt of
        Normal -> do
          modify (first (\_ -> next))
        Letter ch -> do
          tell (DL.singleton ch, mempty)
          modify (first (\_ -> next))
        Crossing -> do
          d <- gets snd
          let dL = turnLeft d
              dR = turnRight d
              nextL = applyDir dL next
              nextR = applyDir dR next
          case (mapQuery nextL, mapQuery nextR) of
            (Nothing, Nothing) -> error "nowhere to go"
            (Just _, Just _) -> error "crossing ambiguous"
            (Just _, Nothing) -> put (next, dL)
            (Nothing, Just _) -> put (next, dR)
    Nothing -> pure $ Just ()

instance Solution Day19 where
  solutionRun _ SolutionContext {getInputS, answerShow, answerS} = do
    xs <- lines <$> getInputS
    let mi@MapInfo {miStart} = parseFromRaw xs
        (_, _, (path, Sum ans)) = runRWS (newStep >> untilJust step) mi miStart
    answerS $ DL.toList path
    answerShow ans
