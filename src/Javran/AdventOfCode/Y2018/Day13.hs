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
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2018.Day13
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Cont
import Control.Monad.State.Strict
import Data.Char
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Debug.Trace
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day13 deriving (Generic)

data Dir = U | L | D | R deriving (Show, Ord, Eq, Enum)

applyDir :: Dir -> Coord -> Coord
applyDir = \case
  U -> second pred
  L -> first pred
  D -> second succ
  R -> first succ

turnLeft, turnRight :: Dir -> Dir
(turnLeft, turnRight) = (\d -> ds !! (fromEnum d + 1), \d -> ds !! (fromEnum d + 3))
  where
    ds = cycle [U .. R]

{-
  coord system:
  o ----- > x
  |
  |
  V
  y

 -}

type Coord = (Int, Int) -- x and y

data Sym
  = SCurve Bool {- False: '\', True: '/' -}
  | SCart Dir
  | SIntersect
  | SPipe Bool {- False: '-', True: '|' -}
  deriving (Show)

data MapInfo = MapInfo
  { miGraph :: M.Map Coord [Coord]
  , miCurves :: M.Map Coord Bool
  , miCarts :: [(Coord, Dir)]
  }
  deriving (Show)

data CartState = CartState
  { csLoc :: Coord -- current location
  , csDir :: Dir -- current direction
  , csTurnOpt :: [Dir -> Dir] -- infinite list of turning decisions, to be used on intersections
  }

tickCart :: MapInfo -> CartState -> CartState
tickCart
  MapInfo {miGraph, miCurves}
  csOld@CartState {csLoc = oldLoc, csDir = oldDir, csTurnOpt = oldTurnOpt}
    | atIntersect =
      let f : csTurnOpt = oldTurnOpt
       in CartState {csLoc, csDir = f oldDir, csTurnOpt}
    | Just curveType <- atCurve =
      let csDir =
            -- TODO: need some clarification here as this is indeed very confusing.
            case curveType of
              False ->
                -- '\'
                case oldDir of
                  U -> L
                  D -> R
                  L -> U
                  R -> D
              True ->
                -- '/'
                case oldDir of
                  U -> R
                  D -> L
                  L -> D
                  R -> U
       in CartState {csLoc, csTurnOpt = oldTurnOpt, csDir}
    | otherwise = csOld {csLoc}
    where
      atCurve :: Maybe Bool
      atCurve = miCurves M.!? csLoc
      atIntersect = case miGraph M.! csLoc of
        [_, _, _, _] -> True
        _ -> False
      csLoc = case miGraph M.!? newLoc of
        Just _ -> newLoc
        Nothing -> error $ "Failed: " <> show (oldLoc, oldDir)
        where
          newLoc = applyDir oldDir oldLoc

parseSym :: Char -> Maybe Sym
parseSym = \case
  ' ' -> Nothing
  '\\' -> Just $ SCurve False
  '/' -> Just $ SCurve True
  '+' -> Just $ SIntersect
  '^' -> Just $ SCart U
  '<' -> Just $ SCart L
  'v' -> Just $ SCart D
  '>' -> Just $ SCart R
  '-' -> Just $ SPipe False
  '|' -> Just $ SPipe True
  s -> error $ "Unknown: " <> show s

parseFromRaw :: [String] -> MapInfo
parseFromRaw raw = MapInfo {miGraph, miCurves, miCarts}
  where
    miCurves = M.fromList do
      (coord, SCurve v) <- M.toList syms
      pure (coord, v)
    miCarts = do
      (coord, SCart d) <- M.toList syms
      pure (coord, d)
    miGraph = M.fromList do
      (coord, sym) <- M.toList syms
      let -- checks connectivity from current coord to a neighborhood.
          conn dir = case syms M.!? coord' of
            Nothing -> False
            Just s' -> case s' of
              SCurve _ -> True
              SIntersect -> True
              SCart d' ->
                if expectVertical
                  then d' `elem` [U, D]
                  else d' `elem` [L, R]
              SPipe vert -> if expectVertical then vert else not vert
            where
              expectVertical = dir `elem` [U, D]
              coord' = applyDir dir coord
      -- TODO: there got to be a better way of writing this ...
      let connInfo@[u, l, d, r] = fmap conn [U, L, D, R]
          errConn =
            error $
              "unexpected connectivity on " <> show (coord, sym) <> " : " <> show connInfo
          handlePipe vert =
            {-
              pipes only care about its own directions, otherwise cases with curves would be tricky,
              e.g.
               \
              ---
               /
             -}
            if vert
              then
                (if u && d
                   then pure (coord, fmap (`applyDir` coord) [U, D])
                   else errConn)
              else
                (if l && r
                   then pure (coord, fmap (`applyDir` coord) [L, R])
                   else errConn)
      case sym of
        SCurve False ->
          -- '\' connects either U,R or L,D, make sure it's mutual exclusive.
          if
              | u && r && not (l && d) ->
                pure (coord, fmap (`applyDir` coord) [U, R])
              | not (u && r) && l && d ->
                pure (coord, fmap (`applyDir` coord) [L, D])
              | otherwise -> errConn
        SCurve True ->
          -- '/' connects either U,L or D,R, make sure it's mutual exclusive.
          if
              | u && l && not (d && r) ->
                pure (coord, fmap (`applyDir` coord) [U, L])
              | not (u && l) && d && r ->
                pure (coord, fmap (`applyDir` coord) [D, R])
              | otherwise -> errConn
        SIntersect -> do
          if and connInfo
            then pure (coord, fmap (`applyDir` coord) [U, L, D, R])
            else errConn
        SPipe vert -> handlePipe vert
        SCart dir -> handlePipe (dir `elem` [U, D])
    syms :: M.Map Coord Sym
    syms = M.fromList do
      (y, rs) <- zip [0 ..] raw
      (x, ch) <- zip [0 ..] rs
      Just s <- pure $ parseSym ch
      pure ((x, y), s)

tick :: Bool -> MapInfo -> StateT (M.Map Coord CartState) (Either Coord) ()
tick stopOnConflict mi = do
  carts <- gets (sortOn (\((x, y), _) -> (y, x)) . M.toList)
  forM_ carts $ \(cartLoc, cartState) -> do
    exist <- gets (M.member cartLoc)
    when exist do
      modify (M.delete cartLoc)
      let cartState'@CartState {csLoc = newLoc} = tickCart mi cartState
      v <- gets (M.!? newLoc)
      case v of
        Just _ -> do
          when stopOnConflict do
            lift $ Left newLoc
          modify (M.delete newLoc)
        Nothing ->
          modify (M.insert newLoc cartState')
  sz <- gets M.size
  when (sz == 1) do
    ~[(c, _)] <- gets M.toList
    lift $ Left c

instance Solution Day13 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerS} = do
    xs <- lines <$> getInputS
    let mi = parseFromRaw xs
        initSt =
          M.fromList
            (fmap
               (\(coord, dir) ->
                  ( coord
                  , CartState
                      { csLoc = coord
                      , csDir = dir
                      , csTurnOpt = cycle [turnLeft, id, turnRight]
                      }
                  ))
               (miCarts mi))

    do
      let Left (x, y) = evalStateT (forever (tick True mi)) initSt
      answerS $ show x <> "," <> show y
    do
      {- TODO: p2 example -}
      let Left (x, y) = evalStateT (forever (tick False mi)) initSt
      answerS $ show x <> "," <> show y
