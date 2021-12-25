{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Javran.AdventOfCode.Y2021.Day21
  (
  )
where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Coerce
import qualified Data.IntMap.Strict as IM
import Data.List
import qualified Data.Map.Strict as M
import Data.Monoid
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day21 deriving (Generic)

newtype Player = Player Bool
  deriving (Eq, Ord) via Bool

pattern P1, P2 :: Player
pattern P1 = Player False
pattern P2 = Player True

{-# COMPLETE P1, P2 #-}

instance Show Player where
  show = \case
    P1 -> "P1"
    P2 -> "P2"

playerP :: ReadP (Int, Int)
playerP =
  (,) <$> (string "Player " *> decimal1P)
    <*> (string " starting position: " *> decimal1P)

type PlayerState = (Int, Sum Int)

type GameState =
  ( [(Int, Int)] -- die sequence
  , (PlayerState, PlayerState)
  )

_p :: Player -> Lens' (a, a) a
_p = \case
  P1 -> _1
  P2 -> _2

not' :: Player -> Player
not' = coerce not

applyStep :: Int -> PlayerState -> PlayerState
applyStep incr (pos, sc) = (pos', sc <> Sum pos')
  where
    pos' = let x = (pos + incr) `rem` 10 in if x == 0 then 10 else x

stepPlayer :: Player -> State GameState (Maybe Player)
stepPlayer who = do
  pt <- sum . fmap snd <$> roll
  advance pt
  score <- gets (snd . view _who . snd)
  pure $ who <$ guard (score >= 1000)
  where
    _who = _p who
    roll = state \(die, p) ->
      -- https://www.youtube.com/watch?v=dQw4w9WgXcQ
      let (ls, die') = splitAt 3 die
       in (ls, (die', p))
    advance pt =
      modify (second (& _who %~ applyStep pt))

stepTillConclusion :: [Player] -> State GameState Player
stepTillConclusion = \case
  [] -> error "expected infinite list"
  (p : ps) ->
    stepPlayer p
      >>= maybe
        -- no conclusion
        (stepTillConclusion ps)
        -- winner found
        pure

type GameState2 = (Player, (PlayerState, PlayerState))

oneStep :: [(Int, Int)]
oneStep = IM.toList $ IM.fromListWith (+) do
  x <- sum <$> replicateM 3 [1, 2, 3]
  pure (x, 1)

stepUniverse :: GameState2 -> M.Map GameState2 Int
stepUniverse (p, ps) = M.fromListWith (+) do
  (step, univCount) <- oneStep
  let ps' = ps & _p p %~ applyStep step
  pure ((not' p, ps'), univCount)

{-
  Steps one turn forward, and discharges universes that have concluded.
 -}
stepMultiverse
  :: M.Map GameState2 Int
  -> ( (Sum Int, Sum Int)
     , M.Map GameState2 Int
     )
stepMultiverse m = (w, inconclusives)
  where
    w =
      mconcat
        . mapMaybe
          (\(univ, count) -> do
             winner <- getWinner univ
             pure $ (0, 0) & _p winner .~ Sum count)
        $ M.toList conclusives
    (conclusives, inconclusives) =
      M.partitionWithKey
        (\k _v -> isJust (getWinner k))
        stepped

    getWinner (_, ((_, p1Score), (_, p2Score))) =
      (P1 <$ guard (p1Score >= 21))
        <|> (P2 <$ guard (p2Score >= 21))

    stepped = M.unionsWith (+) do
      (univ, count) <- M.toList m
      pure $ M.map (* count) $ stepUniverse univ

instance Solution Day21 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    [(1, p1), (2, p2)] <- fmap (consumeOrDie playerP) . lines <$> getInputS
    let initPlayerSt = ((p1, 0), (p2, 0))
    do
      let initSt =
            ( zip [0 ..] (cycle [1 .. 100])
            , initPlayerSt
            )
          (winner, ((dieCount, _) : _, players)) =
            runState (stepTillConclusion (cycle [P1, P2])) initSt
          loser :: Player
          loser = not' winner
          (_, Sum loserScore) = players ^. _p loser
      answerShow (dieCount * loserScore)
    let initAllUniv = M.singleton (P1, initPlayerSt) 1
        univs =
          unfoldr
            (\univ ->
               stepMultiverse univ
                 <$ guard (not $ null univ))
            initAllUniv
    answerShow $ let (Sum l, Sum r) = mconcat univs in max l r
