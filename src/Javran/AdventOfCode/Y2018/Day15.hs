{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2018.Day15
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Cont
import Control.Monad.State.Strict
import Data.Char
import Data.Containers.ListUtils (nubOrd)
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Debug.Trace
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import System.IO.Unsafe (unsafePerformIO)
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day15 deriving (Generic)

{-
  Note that for this one *y goes first, then x*
  This is so that we have Ord instance being consistent with "reading order".
 -}
type Coord = (Int, Int)

{-
  storing mapping from a unit to its hitpoint.

  Hitpoints = (<Hitpoints of Elves>, <Hitpoints of Goblins>)
 -}
type HpState = M.Map Coord Int

type Hitpoints = (HpState, HpState)

type Graph = M.Map Coord [Coord]

data Action
  = Move Coord
  | Attack Coord
  | MoveAttack Coord Coord
  | EndTurn
  | EndCombat
  deriving (Show)

parseFromRaw :: [String] -> (Graph, Hitpoints)
parseFromRaw raw =
  ( g
  , ( M.fromList $ concat elves
    , M.fromList $ concat goblins
    )
  )
  where
    g = M.fromListWith (error "no conflict") do
      coord@(y, x) <- opens
      -- TODO: somehow flatten this gives us the wrong order?
      let coordNext = do
            -- explicit listing to make sure it's in reading order.
            coord' <- [(y -1, x), (y, x -1), (y, x + 1), (y + 1, x)]
            coord' <$ (guard $ S.member coord' opensSet)
      pure (coord, coordNext)
    (elves, goblins) = unzip combatUnits
    opensSet = S.fromList opens
    (opens, combatUnits) = unzip do
      (y, rs) <- zip [0 ..] raw
      (x, ch) <- zip [0 ..] rs
      guard $ ch `elem` ".EG"
      let coord = (y, x)
          em = [(coord, 200 :: Int) | ch == 'E']
          gm = [(coord, 200 :: Int) | ch == 'G']
      pure (coord, (em, gm))

pprGame :: Graph -> GameState -> IO ()
pprGame g GameState {gsHps = (elves, goblins), gsRound} = do
  let Just (MinMax2D ((minY, maxY), (minX, maxX))) =
        foldMap (Just . minMax2D) $ M.keys g
      units :: M.Map Coord (Either Int Int)
      units = M.union (M.map Left elves) (M.map Right goblins)
  putStrLn ("After round " <> show gsRound)
  forM_ [minY - 1 .. maxY + 1] \y -> do
    let render x
          | Just _ <- elves M.!? coord = "E"
          | Just _ <- goblins M.!? coord = "G"
          | Just _ <- g M.!? coord = " "
          | otherwise = "█"
          where
            coord = (y, x)
        rowUnits =
          fmap snd $
            sortOn fst $ fmap (\((y', x'), hp) -> (x', hp)) $ M.toList $ M.filterWithKey (\(y', _) _ -> y' == y) units
        rowExtra = case rowUnits of
          [] -> ""
          _ : _ ->
            " "
              <> intercalate
                ", "
                (fmap
                   (\case
                      Left v -> "E:" <> show v
                      Right v -> "G:" <> show v)
                   rowUnits)
    putStrLn $ (concatMap render [minX - 1 .. maxX + 1]) <> rowExtra

findPath g isAvailable goals q0 discovered = case q0 of
  Seq.Empty ->
    Nothing
  (cur, pRev) Seq.:<| q1 ->
    if
        | S.member cur goals -> Just (reverse pRev)
        | otherwise ->
          let nexts = do
                coord' <- g M.! cur
                guard $ isAvailable coord' && S.notMember coord' discovered
                pure (coord', coord' : pRev)
              discovered' = foldr S.insert discovered (fmap fst nexts)
              q2 = q1 <> Seq.fromList nexts
           in findPath g isAvailable goals q2 discovered'

-- computes the action of a unit
unitAction :: Graph -> HpState -> HpState -> Coord -> Action
unitAction graph friends enemies myCoord = either id id do
  when (null enemies) do
    Left EndCombat
  let isAvailable coord =
        M.notMember coord friends && M.notMember coord enemies
      moveDsts = S.filter (\c -> c == myCoord || isAvailable c) $ S.fromList do
        ec <- M.keys enemies
        -- get adjacents of this enemy
        graph M.! ec
  when (S.member myCoord moveDsts) do
    -- already near, perform attack.
    let possibleTargets = do
          c' <- graph M.! myCoord
          Just eHp <- pure $ enemies M.!? c'
          pure (eHp, c')
        minEnemyHp = minimum (fmap fst possibleTargets)
        -- just pick the first one available - this should already be in reading order.
        (_, target) : _ = filter ((== minEnemyHp) . fst) possibleTargets
    Left $ Attack target
  -- no target immediately available, go to one.
  case findPath graph isAvailable moveDsts (Seq.singleton (myCoord, [])) (S.singleton myCoord) of
    Nothing -> Right EndTurn
    Just ~(mv : _) ->
      let possibleTargets = do
            c' <- graph M.! mv
            Just eHp <- pure $ enemies M.!? c'
            pure (eHp, c')
       in Right case possibleTargets of
            [] -> Move mv
            _ : _ ->
              let minEnemyHp = minimum (fmap fst possibleTargets)
                  (_, target) : _ = filter ((== minEnemyHp) . fst) possibleTargets
               in MoveAttack mv target

data GameState = GameState
  { gsHps :: Hitpoints
  , gsRound :: Int
  }

performRound :: Graph -> ContT a (State GameState) Bool
performRound g = callCC \done -> do
  (elves, goblins) <- gets gsHps
  forM_
    (M.toList $
       M.unionWithKey
         (\k _ _ -> error $ "duplicated: " <> show k)
         (M.map (const True) elves)
         (M.map (const False) goblins))
    \(coord, isElf) -> do
      let _friend = if isElf then _1 else _2
          _enemy = if isElf then _2 else _1
      isAlive <- gets (M.member coord . (^. _friend) . gsHps)
      when isAlive do
        (friends, enemies) <- gets (((,) <$> (^. _friend) <*> (^. _enemy)) . gsHps)
        let action = unitAction g friends enemies coord
        case action of
          EndCombat -> done True
          EndTurn -> pure ()
          Move coord' ->
            let f m = M.insert coord' hp $ M.delete coord m
                  where
                    hp = m M.! coord
             in modify (\s -> s {gsHps = gsHps s & _friend %~ f})
          Attack coord' ->
            let f =
                  M.alter
                    (\case
                       Nothing -> Nothing
                       Just v -> let v' = v - 3 in v' <$ guard (v' > 0))
                    coord'
             in modify (\s -> s {gsHps = gsHps s & _enemy %~ f})
          MoveAttack mvTo attackAt -> do
            let f0 m = M.insert mvTo hp $ M.delete coord m
                  where
                    hp = m M.! coord
            modify (\s -> s {gsHps = gsHps s & _friend %~ f0})
            let f1 =
                  M.alter
                    (\case
                       Nothing -> Nothing
                       Just v -> let v' = v - 3 in v' <$ guard (v' > 0))
                    attackAt
            modify (\s -> s {gsHps = gsHps s & _enemy %~ f1})

  modify (\s -> s {gsRound = gsRound s + 1})
  pure False

-- simulate :: Graph -> ContT GameState (State GameState) GameState
simulate g = do
  end <- performRound g
  if end
    then get
    else simulate g

instance Solution Day15 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- lines <$> getInputS
    let (g, hps) = parseFromRaw xs
        initSt = GameState {gsHps = hps, gsRound = 0}
        -- (_, r) = runState (runContT (performRound g) pure) initSt
        (_, fin@GameState {gsRound, gsHps = (es, gs)}) = runState (runContT (simulate g) pure) initSt
        outcome = gsRound * (sum es + sum gs)
    --
    pprGame g fin
    answerShow outcome
