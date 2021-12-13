{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Javran.AdventOfCode.Y2019.Day13
  (
  )
where

import Control.Monad
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Function
import qualified Data.Map.Strict as M
import Data.Semigroup
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2019.IntCode

data Day13 deriving (Generic)

type ScreenDim = ((Int, Int), (Int, Int))

type Coord = (Int, Int)

type ScreenState = (Maybe Int, M.Map Coord Int)

printScreen :: ScreenDim -> ScreenState -> IO ()
printScreen ((minX, maxX), (minY, maxY)) (mScore, screen) = do
  forM_ [minY .. maxY] $ \y -> do
    let render coord = case screen M.!? coord of
          Just 0 -> "  "
          Just 1 -> "██"
          Just 2 -> "░░"
          Just 3 -> "━━"
          Just 4 -> "()"
          _ -> "  "
    putStrLn $
      concatMap
        (\x -> render (x, y))
        [minX .. maxX]
  putStrLn $
    "Current score: " <> case mScore of
      Nothing -> "?"
      Just x -> show x

data GameState = GameState
  { gsScreen :: M.Map Coord Int
  , gsScore :: Maybe Int
  , gsPaddleX :: Maybe Int
  , gsBallX :: Maybe Int
  }

playGame :: ScreenDim -> IO (Result a) -> StateT GameState IO (Maybe Int)
playGame screenDim prog = do
  r <- liftIO prog
  case r of
    Done {} ->
      gets gsScore
    NeedInput k -> do
      GameState {gsPaddleX, gsBallX}<- get
      let
          i = case (gsPaddleX, gsBallX) of
            (Just pX, Just bX) -> case compare pX bX of
              LT -> 1
              EQ -> 0
              GT -> -1
            _ -> 0
      playGame screenDim (k i)
    SentOutput {} -> do
      ([x, y, val], k) <- liftIO $ communicate [] 3 (pure r)
      if (x, y) == (-1, 0)
        then modify (\gs -> gs {gsScore = Just val})
        else do
          case val of
            3 ->
             modify (\gs -> gs {gsPaddleX = Just x})
            4 ->
             modify (\gs -> gs {gsBallX = Just x})
            _ -> pure ()
          modify (\gs -> gs {gsScreen = M.insert (x, y) val (gsScreen gs)})
      playGame screenDim k

instance Solution Day13 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow, terminal} = do
    xs <- parseCodeOrDie <$> getInputS
    screenDim <- do
      let prog = startProgramFromFoldable xs
      screen <-
        fix
          (\loop curProg acc -> do
             r <- curProg
             case r of
               Done {} -> pure acc
               NeedInput {} -> errInvalid
               SentOutput {} -> do
                 ([x, y, tileId], k) <- communicate [] 3 (pure r)
                 loop k (M.insert (x, y) tileId acc))
          prog
          M.empty
      let Just ((Min minX, Max maxX), (Min minY, Max maxY)) =
            foldMap (\(x, y) -> Just ((Min x, Max x), (Min y, Max y))) $
              M.keys screen
      -- it seems to be a safe assumption that we can carry screen dimension over to part 2.
      ((minX, maxX), (minY, maxY))
        <$ answerShow (M.size $ M.filter (== 2) screen)
    print screenDim
    do
      let xs' = 2 : tail xs
          prog = startProgramFromFoldable xs'
          initSt =
            GameState
              { gsScreen = M.empty
              , gsScore = Nothing
              , gsPaddleX = Nothing
              , gsBallX = Nothing
              }
      Just v <- evalStateT (playGame screenDim prog) initSt
      answerShow v
