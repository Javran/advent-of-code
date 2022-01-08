{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Javran.AdventOfCode.Y2018.Day17
  (
  )
where

import Control.Monad
import Control.Monad.State.Strict
import qualified Data.Set as S
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day17 deriving (Generic)

type Coord = (Int, Int) -- x, y

type CoordSet = S.Set Coord

inputLineP :: ReadP [Coord]
inputLineP = do
  (cNext, mk) <-
    (( 'y'
     , \x ys -> [(x, y) | y <- ys]
     )
       <$ char 'x')
      <++ (( 'x'
           , \y xs -> [(x, y) | x <- xs]
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
  State for a simulation approach: we maintain a set of springs
  and simulate as they go downwards.
  Whenever they hit something "solid" (either clay or still water are considered so).
  and there is no way for the spring to continue going down,
  the simulation goes back up, filling still water until springs can go downwards again.

  A coord of water could be in both wsReached and wsStayed,
  in which case the water on that coord is considered stayed (i.e. still water).
 -}
data WaterState = WaterState
  { wsSpring :: CoordSet
  , wsScanY :: Int -- current scanning line, only coord of that y-coord is considered.
  , wsReached :: CoordSet
  , wsStayed :: CoordSet
  }

{-
  Considers a single spring at (x, wsScanY-1), processes scaning line wsScanY and
  makes a decision about which direction should we go next:
  - True: we hit some solid and current scanning line is filled,
    and we need to go back up.
  - False: this spring will be able to go downwards without obstacle.
 -}
simulateOneSpring :: S.Set Coord -> Int -> State WaterState Bool
simulateOneSpring clay x = do
  y <- gets wsScanY
  (isSolid :: Coord -> Bool) <-
    gets
      ((\stayed coord ->
          {-
            either clay or still water can stop the spring,
            causing it to move horizontally.
           -}
          S.member coord stayed || S.member coord clay)
         . wsStayed)
  let curSpring = (x, y -1)
      replaceSpring xs =
        modify
          (\ws@WaterState {wsSpring} ->
             ws {wsSpring = foldr S.insert (S.delete curSpring wsSpring) xs})
      markReached xs = modify (\ws@WaterState {wsReached} -> ws {wsReached = foldr S.insert wsReached xs})
      markStayed xs = modify (\ws@WaterState {wsStayed} -> ws {wsStayed = foldr S.insert wsStayed xs})
  if isSolid (x, y + 1)
    then do
      -- spring is hitting something solid, now we want to reach both left and right.
      let {-
            reaching to leftmost or rightmost on row y,
            where spring will either be able to continue downwards or
            be blocked.

            Returns (<farthest>, <is open?>), where <farthest> indicates the leftmost
            or rightmost that water can reach, and <is open?> indicates whether
            spring can go downwards.
           -}
          reaching next curX =
            if
                | isSolid (nextX, y) -> (curX, False)
                | isSolid (nextX, y + 1) -> reaching next nextX
                | otherwise -> (nextX, True)
            where
              nextX = next curX
          (leftmost, openL) = reaching pred x
          (rightmost, openR) = reaching succ x
          newCoords = [(x', y) | x' <- [leftmost .. rightmost]]
      case (openL, openR) of
        (False, False) -> do
          -- both end closed, we need to go back up.
          -- (x, y-1) ==> (x, y-1)
          replaceSpring [(x, y -2)]
          True <$ markStayed newCoords
        (True, True) -> do
          {-
            It should be possible to generalize cases other than (False, False),
            but I feel it's most readable to call it out what to do for all 3 cases.
           -}
          markReached newCoords
          -- both open, split the spring.
          False <$ replaceSpring [(leftmost, y), (rightmost, y)]
        (False, True) -> do
          markReached newCoords
          -- left closed, right open, move to right.
          False <$ replaceSpring [(rightmost, y)]
        (True, False) -> do
          -- left open, right closed, move to left.
          markReached newCoords
          False <$ replaceSpring [(leftmost, y)]
    else do
      -- spring is not hiting anything solid, mark it reached and carry current spring downwards.
      -- (x, y-1) ==> (x, y)
      replaceSpring [(x, y)]
      False
        <$ modify
          (\ws@WaterState {wsReached} ->
             ws {wsReached = S.insert (x, y) wsReached})

simulate :: S.Set Coord -> Int -> State WaterState ()
simulate clay yMax = do
  y <- gets wsScanY
  when (y <= yMax) do
    xs <- gets (concatMap (\(x', y') -> [x' | y' == y - 1]) . S.toList . wsSpring)
    shouldGoUp <- forM xs \x -> do
      exist <- gets (S.member (x, y -1) . wsSpring)
      if exist
        then simulateOneSpring clay x
        else pure False
    modify (\ws@WaterState {wsScanY} -> ws {wsScanY = if or shouldGoUp then wsScanY -1 else wsScanY + 1})
    simulate clay yMax

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
    putStrLn $ "<<" <> fmap render [xMin .. xMax] <> ">>"

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
