{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Javran.AdventOfCode.Y2015.Day21
  (
  )
where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.State.Strict
import Data.List
import Data.Monoid hiding (First, Last)
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.TestExtra
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day21 deriving (Generic)

-- short for Hit points, Damage, and Armor
type Hda = (Int, (Int, Int))

hdaP :: ReadP Hda
hdaP = do
  let nl = char '\n'
  h <- strP "Hit Points: " *> decimal1P <* nl
  d <- strP "Damage: " *> decimal1P <* nl
  a <- strP "Armor: " *> decimal1P <* nl
  pure (h, (d, a))

attack :: Hda -> Hda -> Maybe Hda
attack (_, (dmg, _)) (eeHp, eeK@(_, eeArmor)) =
  (eeHp', eeK) <$ guard (eeHp' > 0)
  where
    eeHp' = eeHp - dmgDealt
    dmgDealt = max (dmg - eeArmor) 1

data Side = Player | Boss deriving (Show, Eq)

simulate :: Side -> State (Hda, Hda) (Maybe Side)
simulate attackingSide = do
  let _ker = case attackingSide of
        Player -> _1
        Boss -> _2
      _kee = case attackingSide of
        Player -> _2
        Boss -> _1
  (ker, kee) <- gets (\v -> (v ^. _ker, v ^. _kee))
  case attack ker kee of
    Nothing ->
      -- attackee is dead.
      pure $ Just attackingSide
    Just kee' -> do
      modify (& _kee .~ kee')
      pure Nothing

pickInOrderUpToN :: [a] -> Int -> [[a]]
pickInOrderUpToN xs n =
  pure [] <|> do
    guard $ n > 0
    (y, ys) <- pickInOrder xs
    (y :) <$> pickInOrderUpToN ys (n -1)

-- short for Cost, Damage, and Armor
type Cda = (Int, (Int, Int))

type Item = (String, Cda)

weapons, armors, rings :: [Item]
(weapons, armors, rings) = (ws, ams, rs)
  where
    ws =
      [ t "Dagger" 8 4 0
      , t "Shortsword" 10 5 0
      , t "Warhammer" 25 6 0
      , t "Longsword" 40 7 0
      , t "Greataxe" 74 8 0
      ]
    ams =
      [ t "Leather" 13 0 1
      , t "Chainmail" 31 0 2
      , t "Splintmail" 53 0 3
      , t "Bandedmail" 75 0 4
      , t "Platemail" 102 0 5
      ]
    rs =
      [ t "Damage +1" 25 1 0
      , t "Damage +2" 50 2 0
      , t "Damage +3" 100 3 0
      , t "Defense +1" 20 0 1
      , t "Defense +2" 40 0 2
      , t "Defense +3" 80 0 3
      ]
    t w c d a = (w, (c, (d, a)))

players :: [(Int, (Hda, [String]))]
players = sortOn fst do
  -- exactly one weapon
  w <- weapons
  -- up to one armor
  pAs <- pickInOrderUpToN armors 1
  -- up to two rings
  pRs <- pickInOrderUpToN rings 2
  let pItems :: [Item]
      pItems = w : pAs <> pRs
      (Sum total, (Sum pD, Sum pA)) =
        foldMap ((\(c, (d, a)) -> (Sum c, (Sum d, Sum a))) . snd) pItems
      itemNames :: [String]
      itemNames = fmap fst pItems
      hda = (100, (pD, pA))
  pure (total, (hda, itemNames))

instance Solution Day21 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (ex, rawInput) <- consumeExtra getInputS
    let boss = consumeOrDie hdaP rawInput
        (runPart1, runPart2) = shouldRun ex
        playWith :: Hda -> Side
        playWith player =
          evalState
            (fix
               (\go (y : ys) ->
                  y >>= \case
                    Nothing -> go ys
                    Just w -> pure w)
               $ fmap simulate (cycle [Player, Boss]))
            (player, boss)
    when runPart1 do
      answerShow $ head do
        (cost, (player, _)) <- players
        guard $ playWith player == Player
        pure cost
    when runPart2 do
      answerShow $ head do
        (cost, (player, _)) <- reverse players
        guard $ playWith player == Boss
        pure cost
