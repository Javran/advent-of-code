{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2020.Day22
  (
  )
where

import Control.Monad.State.Strict
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude

data Day22 deriving (Generic)

type GameState = ([Int], [Int])

data Player = P1 | P2

stepGame :: GameState -> Maybe GameState
stepGame gs = do
  (x : xs, y : ys) <- pure gs
  case compare x y of
    EQ -> undefined
    LT -> Just (xs, ys <> [y, x])
    GT -> Just (xs <> [x, y], ys)

type GameHistory = S.Set GameState

type G = State GameHistory

playGame2 :: GameState -> G (Player, GameState)
playGame2 gs = do
  r <- stepGame2 gs
  case r of
    Left winner -> pure (winner, gs)
    Right gs' -> playGame2 gs'

stepGame2 :: GameState -> G (Either Player GameState)
stepGame2 gs = do
  occurred <- gets (S.member gs)
  modify (S.insert gs)
  if occurred
    then pure (Left P1)
    else case gs of
      ([], _) -> pure (Left P2)
      (_, []) -> pure (Left P1)
      (x : xs, y : ys) -> do
        if length xs >= x && length ys >= y
          then
            let (subGameWinner, _) =
                  evalState (playGame2 (take x xs, take y ys)) S.empty
             in pure $
                  Right
                    case subGameWinner of
                      P2 ->
                        (xs, ys <> [y, x])
                      P1 ->
                        (xs <> [x, y], ys)
          else case compare x y of
            EQ -> undefined
            LT -> pure $ Right (xs, ys <> [y, x])
            GT -> pure $ Right (xs <> [x, y], ys)

computeWinnerScore :: GameState -> Int
computeWinnerScore (ls, rs) = sum $ zipWith (*) [1 ..] (reverse $ ls <> rs)

instance Solution Day22 where
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
    answerShow $ computeWinnerScore result
    let (_w, gs2) = evalState (playGame2 (xs, ys)) S.empty
    answerShow $ computeWinnerScore gs2
