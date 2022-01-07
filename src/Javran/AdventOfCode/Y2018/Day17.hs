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

module Javran.AdventOfCode.Y2018.Day17
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
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
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day17 deriving (Generic)

type Coord = (Int, Int) -- x, y

inputLineP :: ReadP [Coord]
inputLineP = do
  (cNext, mk) <-
    (( 'y'
     , \x ys -> [(x, y) | y <- ys]
     )
       <$ char 'x')
      <++ (( 'x'
           , (\y xs -> [(x, y) | x <- xs])
           )
             <$ char 'y')
  _ <- char '='
  u <- decimal1P
  _ <- string (", " <> [cNext] <> "=")
  vFrom <- decimal1P
  _ <- string ".."
  vTo <- decimal1P
  pure (mk u [vFrom .. vTo])

{-
  TODO: I suspect we are looking at some simulation again:

  - keep a list of x of springs (resulting from branching the very first one),
    and y-coord that we are currently scanning.
  - moving down until we "hit" something, set `|` mark appropriately.
  - now we have few situations to deal with

    + if we cannot find bound on either side, find locations that we can
      branch this spring and keep going down, set `|` mark appropriately.

    + if we find bound one side, set `|` mark appropriately,
      and "redirect" current spring, as we are leaking from only one side.

    + if we find bound both sides, set `~` as appropriate, then
      we have to move y back up instead of down to "fill the reservoir"

 -}

{-
  a coord could be in both, wsReached and wsStayed.
  in which case that coord is considered stayed.
 -}
data WaterState = WaterState
  { wsSpring :: S.Set Coord
  , wsScanY :: Int -- current scanning line, only coord of that y-coord is considered.
  , wsReached :: S.Set Coord
  , wsStayed :: S.Set Coord
  }

simulate :: S.Set Coord -> Int -> State WaterState ()
simulate clay yMax = do
  y <- gets wsScanY
  when (y <= yMax) do
    xs <- gets (concatMap (\(x', y') -> [x' | y' == y - 1]) . S.toList . wsSpring)
    shouldGoUp <- forM xs \x -> do
      exist <- gets ((S.member (x, y -1)) . wsSpring)
      if exist
        then simulateAux clay x
        else pure False
    modify (\ws@WaterState {wsScanY} -> ws {wsScanY = if or shouldGoUp then wsScanY -1 else wsScanY + 1})
    simulate clay yMax

{-
  considers a single spring x, processes it and return whether we want to "rewind" y.
  (True means we want to rewind)
 -}
simulateAux :: S.Set Coord -> Int -> State WaterState Bool
simulateAux clay x = do
  y <- gets wsScanY
  (isSolid :: Coord -> Bool) <-
    gets
      ((\stayed coord -> S.member coord stayed || S.member coord clay)
         . wsStayed)
  let curSpring = (x, y -1)
  if isSolid (x, y + 1)
    then do
      -- solid, now we want to reach both left and right.
      let reaching next curX =
            if
                | isSolid (nextX, y) -> (curX, False) -- (<farthest reach>, <is open?>)
                | isSolid (nextX, y + 1) -> reaching next nextX
                | otherwise -> (nextX, True)
            where
              nextX = next curX
          (leftmost, openL) = reaching pred x
          (rightmost, openR) = reaching succ x
          newCoords = [(x', y) | x' <- [leftmost .. rightmost]]
          markReached = modify (\ws@WaterState {wsReached} -> ws {wsReached = foldr S.insert wsReached newCoords})
      case (openL, openR) of
        (False, False) -> do
          -- both end closed, we need to go back up.
          modify (\ws@WaterState {wsSpring} -> ws {wsSpring = S.insert (x, y -2) $ S.delete curSpring wsSpring})
          True <$ modify (\ws@WaterState {wsStayed} -> ws {wsStayed = foldr S.insert wsStayed newCoords})
        (True, True) -> do
          markReached
          -- both open, split the spring.
          False
            <$ modify
              (\ws@WaterState {wsSpring} ->
                 ws
                   { wsSpring =
                       S.insert (rightmost, y)
                         . S.insert (leftmost, y)
                         . S.delete curSpring
                         $ wsSpring
                   })
        (False, True) -> do
          markReached
          -- left closed, right open, move to right.
          False
            <$ modify
              (\ws@WaterState {wsSpring} ->
                 ws
                   { wsSpring =
                       S.insert (rightmost, y)
                         . S.delete curSpring
                         $ wsSpring
                   })
        (True, False) -> do
          markReached
          False
            <$ modify
              (\ws@WaterState {wsSpring} ->
                 ws
                   { wsSpring =
                       S.insert (leftmost, y)
                         . S.delete curSpring
                         $ wsSpring
                   })
    else do
      -- not solid, mark reached and move on
      modify (\ws@WaterState {wsSpring} -> ws {wsSpring = S.insert (x, y) $ S.delete curSpring wsSpring})
      False
        <$ modify
          (\ws@WaterState {wsReached} ->
             ws {wsReached = S.insert (x, y) wsReached})

pprWaterState :: S.Set Coord -> WaterState -> IO ()
pprWaterState clay WaterState {wsStayed, wsReached} = do
  let Just (MinMax2D ((xMin, xMax), (yMin, yMax))) = foldMap (Just . minMax2D) (S.toList clay)
  forM_ [yMin .. yMax] \y -> do
    let render x =
          if
              | S.member (x, y) clay -> '█'
              | S.member (x, y) wsStayed -> '~'
              | S.member (x, y) wsReached -> '|'
              | otherwise -> ' '
    putStrLn $ "<<" <> (fmap render [xMin .. xMax]) <> ">>"

instance Solution Day17 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (consumeOrDie inputLineP) . lines <$> getInputS
    let clay = S.fromList (concat xs)
        Just (MinMax2D (_, (yMin, yMax))) = foldMap (Just . minMax2D) (S.toList clay)
        ws = execState (simulate clay yMax) WaterState {wsSpring = S.singleton (500, 0), wsScanY = 1, wsStayed = S.empty, wsReached = S.empty}
        reachables = S.filter (\(_x, y) -> y >= yMin && y <= yMax) $ S.union (wsStayed ws) (wsReached ws)
        stayed = S.filter (\(_x, y) -> y >= yMin && y <= yMax) $ wsStayed ws
        display = False
    when display do
      pprWaterState clay ws
    answerShow (S.size reachables)
    answerShow (S.size stayed)
