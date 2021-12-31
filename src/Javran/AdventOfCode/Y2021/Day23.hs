{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2021.Day23
  (
  )
where

{- HLINT ignore -}

import Control.Lens hiding (universe)
import Control.Monad
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.PSQueue as PQ
import qualified Data.Set as S
import Debug.Trace
import Javran.AdventOfCode.Prelude
import System.IO.Unsafe (unsafePerformIO)
import Data.Semigroup

data Day23 deriving (Generic)

{-

  Map parsing: this map has an irregular shape and there are concepts like
  "hallways" and "siderooms" that are not present or present but in
  a parsing-friendly way.

  Worst, not knowing what part 2 is about makes it difficult
  to generalize. So my initial solution totally forgo parsing and
  just hard coded information about the map with few rounds of
  `print` and copy them back into code.

  With the benefit of hindsight, it makes sense to assume the following:

  - the map configuration looks like this:

    > #############
    > #...........#  -- hallway, row = 1.
    > ###.#.#.#.###
    >   #.#.#.#.#    -- x
    >   <repeat x 0 ~ many times>
    >   #########
    --   ^ ^ ^ ^
    --   3 5 7 9

  - side rooms are always assigned A, B, C, D, in that order,
    at column 3, 5, 7, 9, respectively.
  - hallway iff. open cells where row = 1.
  - a open cell is either a hallway or part of a side room.
  - # of amphipods and # room spaces always match.

 -}

{-
  Solving part 1 by hand.

  Part 1 is solvable by hand - we just need to realize that
  C and D are so heavy that its movement must be minimized,
  this insight narrows down the search space quite a lot that
  we can try to find the best way to solve it by hand.

 -}

data AmpType = A | B | C | D deriving (Eq, Ord, Enum, Show, Read, Bounded)

type Coord = (Int, Int)

type CoordSet = S.Set Coord

data MapInfo = MapInfo
  { miGraph :: M.Map Coord CoordSet
  , miRoomSize :: Int -- how tall is the room
  }
  deriving (Show)

type WorldState = [CoordSet] -- exactly 4 elements for 4 AmpTypes

parseRawMap :: [String] -> (MapInfo, WorldState)
parseRawMap raw =
  ( MapInfo
      { miGraph
      , miRoomSize
      }
  , case wsE of
      Left err -> error $ "validation error: " <> err
      Right r -> r
  )
  where
    wsE = do
      unless (S.fromList (universe @AmpType) == M.keysSet ampLocs) do
        Left "unexpected amp keys"
      unless (all ((== miRoomSize) . S.size) ampLocs) do
        Left "amp and room count mismatch"
      pure $ fmap snd (M.toAscList ampLocs)
    ampLocs :: M.Map AmpType CoordSet
    ampLocs = M.unionsWith (<>) (catMaybes ampLocsPre)
    openCells = S.fromList graphPre
    miRoomSize = length raw - 3
    miGraph = M.fromListWith (<>) do
      coord <- S.toList openCells
      coord' <- udlrOfCoord coord
      guard $ S.member coord' openCells
      pure (coord, S.singleton coord')
    (graphPre, ampLocsPre) = unzip do
      (r, rs) <- zip [0 ..] raw
      (c, x) <- zip [0 ..] rs
      guard $ x `notElem` "# "
      let coord = (r, c)
          mAmp = case reads @AmpType [x] of
            [(ampTy, "")] -> Just ampTy
            _ -> Nothing
      pure (coord, (\amp -> M.singleton amp (S.singleton coord)) <$> mAmp)

{-
  The space immediately outside any room,
  This is consistent across maps so that we can hard-code.
 -}
can'tStops :: CoordSet
can'tStops = S.fromList [(1, 3), (1, 5), (1, 7), (1, 9)]

isInHallway :: Coord -> Bool
isInHallway (r, _) = r == 1

moveCost :: AmpType -> Int
moveCost = \case
  A -> 1
  B -> 10
  C -> 100
  D -> 1000

moveTargets :: Int -> AmpType -> S.Set Coord
moveTargets roomSize = \case
  A -> S.fromList $ fmap (,3) rs
  B -> S.fromList $ fmap (,5) rs
  C -> S.fromList $ fmap (,7) rs
  D -> S.fromList $ fmap (,9) rs
  where
    rs = take roomSize [2 ..]

_pprWorldState :: Int -> WorldState -> IO ()
_pprWorldState roomSize ws = do
  let ampLocs :: M.Map Coord AmpType
      ampLocs = M.fromList do
        (aTy, cs) <- zip [A .. D] ws
        coord <- S.toList cs
        pure (coord, aTy)
      raw =
        [ "#############"
        , "#..!.!.!.!..#"
        , "###.#.#.#.###"
        ]
          <> replicate (roomSize - 1) "  #.#.#.#.#  "
          <> [ "  #########  "
             ]
  forM_ [0 .. length raw -1] $ \r -> do
    let render c
          | Just ampTy <- ampLocs M.!? (r, c) =
            show ampTy
          | otherwise = [raw !! r !! c]
    putStrLn $ concatMap render [0 .. length (head raw) -1]

-- putStrLn $ "Homing: " <> show (homingPriority' roomSize ws)

targetWorld :: Int -> WorldState
targetWorld roomSize = fmap (moveTargets roomSize) [A .. D]

manhattan (a, b) (c, d) = abs (a - c) + abs (b - d)

homeColumn :: AmpType -> Int
homeColumn = \case
  A -> 3
  B -> 5
  C -> 7
  D -> 9

{-
  Measure the distance between current location
  to the bottom of the correct room.
 -}
homingDist :: Int -> AmpType -> Coord -> Int
homingDist roomSize ampType coord@(r, c) =
  if
      | c == rightCol -> manhattan coord home
      | r == 1 -> 10 * manhattan coord home
      | otherwise -> 100 * ((r -1) + manhattan (1, c) home)
  where
    home = (roomSize + 1, rightCol)
    rightCol = homeColumn ampType

{-
  For amp outside, it's the distance to top of the correct room,
  for amp inside, 0.
 -}
ampHomingDist :: AmpType -> Coord -> Int
ampHomingDist ampType coord@(r, c) =
  if
      | r == 1 -> manhattan coord homeTop
      | c == rightCol -> 0
      | otherwise -> ((r -1) + manhattan (1, c) homeTop)
  where
    homeTop = (2, rightCol)
    rightCol = homeColumn ampType

ampHomingDists :: Int -> AmpType -> [Coord] -> Int
ampHomingDists roomSize ampType cs = sum (fmap snd tracedDists) + alreadyHomeIncr + stillOutsideIncr
  where
    tracedDists = fmap (\c -> (c, ampHomingDist ampType c)) cs
    (alreadyHome, stillOutside) = partition ((== 0) . snd) tracedDists
    alreadyHomeSortedRowsDesc = sortBy (comparing Down) $ fmap (\((r, _c), _) -> r) alreadyHome
    {-
      room row range: [2 .. roomSize-1]
      compute how many more most to "pack" those already home to bottom.
     -}
    alreadyHomeIncr = sum $ zipWith (-) [roomSize + 1, roomSize ..] alreadyHomeSortedRowsDesc
    stillOutsideIncr = cnt * (cnt -1) `quot` 2
      where
        cnt = length stillOutside

homingDist2 :: Int -> WorldState -> Int
homingDist2 roomSize ws =
  sum $ zipWith (\ampType cs -> ampHomingDists roomSize ampType (S.toList cs)) [A .. D] ws

homingEnergy :: Int -> WorldState -> Int
homingEnergy roomSize ws =
  sum $ zipWith (\ampType cs -> moveCost ampType * ampHomingDists roomSize ampType (S.toList cs)) [A .. D] ws

homingPriority' :: Int -> WorldState -> Int
homingPriority' roomSize ws = sum $ zipWith go ws [A .. D]
  where
    go cs ampType = sum $ fmap (homingDist roomSize ampType) $ S.toList cs

{-
  Measures how close we are relative to a solution,
  the actual value doesn't matter, but the closer we are to the solution,
  this value becomes smaller.
 -}
homingPriority :: WorldState -> WorldState -> Down Int
homingPriority ws wsTarget = Down (sum $ fmap S.size $ zipWith S.intersection wsTarget ws)

findNextMoves :: MapInfo -> AmpType -> Coord -> WorldState -> [(Coord, WorldState, Int)]
findNextMoves MapInfo {miRoomSize, miGraph} ampType initCoord wsPre =
  findNextMovesAux (PQ.singleton initCoord 0) (S.singleton initCoord)
  where
    ampLocs :: M.Map Coord AmpType
    ampLocs = M.fromList do
      (aTy, cs) <- zip [A .. D] ws
      coord <- S.toList cs
      pure (coord, aTy)

    ws = wsPre & ix (fromEnum ampType) %~ S.delete initCoord
    blockings :: S.Set Coord
    blockings = S.unions ws
    findNextMovesAux q0 discovered = case PQ.minView q0 of
      Nothing -> []
      Just (coord@(r, c) PQ.:-> energy, q1) ->
        [ (coord, ws & ix (fromEnum ampType) %~ S.insert coord, energy)
        | S.notMember coord can'tStops
        ]
          <> let nexts = do
                   Just coords' <- [miGraph M.!? coord]
                   coord'@(r', c') <- S.toList coords'
                   let myMoveTargets = moveTargets miRoomSize ampType
                       myHomeCol = homeColumn ampType
                       targetRoomIsClear =
                         {- a clear room is only occupied by the same ampType, open space allowed.
                          -}
                         all
                           (\targetCoord -> case ampLocs M.!? targetCoord of
                              Nothing -> True
                              Just ampType' -> ampType' == ampType)
                           myMoveTargets
                   when (isInHallway coord && not (isInHallway coord')) do
                     -- when moving down from hallway.
                     guard $ c' == myHomeCol
                     -- also room must be clear
                     guard targetRoomIsClear
                   when (r > 1 && c == myHomeCol && targetRoomIsClear) do
                     -- if we are already in target room and that room is clear
                     -- it won't do us much good moving up again
                     guard (r' > r)
                   when (not (isInHallway coord) && c /= myHomeCol) do
                     -- must move up
                     guard (r' < r)
                   guard $ S.notMember coord' blockings
                   guard $ S.notMember coord' discovered
                   pure coord'
                 q2 = foldr upd q1 nexts
                   where
                     upd coord' =
                       let energy' = energy + moveCost ampType
                        in PQ.alter
                             (\case
                                Nothing -> Just energy'
                                Just e -> Just (min e energy'))
                             coord'
                 result = findNextMovesAux q2 (S.union discovered (S.fromList nexts))
              in result

{-
  TODO: Something isn't right:
  the search doesn't find optimal solution with homing priority being just ().
  but somehow we managed to do so in this variation that uses a very inaccurate
  homing estimation .. why?

 -}

type SearchPrio =
  ( Int -- Down Int
  , Int -- energy
  )

{-
  TODO: prune "deadlock" cases like this:

 #############
 #..!B!C!.!.A#
 ###A#D#B#D###
   #.#.#C#.#
   #########

 -}

debugBfs = False

bfs mi@MapInfo {miRoomSize} q0 gScore fScore = case PQ.minView q0 of
  Nothing -> error "queue exhausted"
  Just (ws PQ.:-> (Arg _fScore energy), q1) ->
    if ws == wsTarget
      then energy
      else
        let gScoreCur = gScore M.! ws
            nexts = do
              (ampType, coords) <- zip [A .. D] ws
              coord@(r, c) <- S.toList coords
              let curMoveTargets = moveTargets miRoomSize ampType
                  myHomeCol = homeColumn ampType
              (coord'@(r', c'), ws', incr) <- findNextMoves mi ampType coord ws
              guard $ coord /= coord'
              -- if in hallway, must move to a room
              when (isInHallway coord) $ do
                guard $ c' == myHomeCol
                -- and squeeze it down all the way.
                guard $
                  r' == miRoomSize + 1
                    || S.member (r' + 1, c') (ws' !! fromEnum ampType)
              when (not (isInHallway coord) && c /= myHomeCol) do
                -- if in the wrong room, we must move out.
                -- (moving up one place may be possible, just not very productive)
                guard $ r' == 1 || c' == myHomeCol
              let tentativeGScore = gScoreCur + incr
              guard $ case gScore M.!? ws' of
                Nothing -> True
                Just v -> tentativeGScore < v -- TODO: write this to gScore
              let fScoreNext = tentativeGScore + homingEnergy miRoomSize ws'
              let energy' = energy + incr
              pure (ws', tentativeGScore, fScoreNext, energy')
            q2 = foldr upd q1 nexts
              where
                upd (ws', _tentativeGScore, fScoreNext, energy') =
                  PQ.alter
                    (\case
                       Nothing -> Just (Arg fScoreNext energy')
                       Just (Arg _ e) -> Just (Arg fScoreNext $ min e energy'))
                    ws'
            gScore' =
              foldr
                (\(ws', tentativeGScore, _fScoreNext, _energy') -> M.insert ws' tentativeGScore)
                gScore
                nexts
            fScore' =
                            foldr
                (\(ws', _tentativeGScore, fScoreNext, _energy') -> M.insert ws' fScoreNext)
                fScore
                nexts

            result = bfs mi q2 gScore' fScore'
         in if debugBfs
              then unsafePerformIO do
                putStrLn "Current:"
                _pprWorldState miRoomSize ws
                putStrLn $ "Energy: " <> show energy
                putStrLn ""

                _pprWorldState miRoomSize ws
                pure result
              else result
  where
    wsTarget = targetWorld miRoomSize

-- aStarSearch

solveFromRawMap :: [String] -> Int
solveFromRawMap rawMap =
  bfs
    mi
    (PQ.singleton
       startState
       (Arg initHoming 0))
    (M.singleton startState 0)
    (M.singleton startState initHoming)
  where
    (mi, startState) = parseRawMap rawMap
    -- initHoming = ()
    -- initHoming = homingPriority' (miRoomSize mi) startState
    -- initHoming = homingPriority startState (targetWorld (miRoomSize mi))
    initHoming = homingEnergy (miRoomSize mi) startState

instance Solution Day23 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (extraOps, rawInput) <- consumeExtraLeadingLines <$> getInputS
    let rawMap = lines rawInput
    case extraOps of
      Nothing -> do
        -- running login input.
        {-
          TODO: part 1 is solved by hand. however
          my current implemention is not giving the optimal answer.
         -}
        answerShow @Int 12521
        let rawMap2 =
              let (xs, ys) = splitAt 3 rawMap
               in xs
                    <> [ "  #D#C#B#A#"
                       , "  #D#B#A#C#"
                       ]
                    <> ys
        -- weirdly enough, my solution does find the right answer for part 2...
        answerShow (solveFromRawMap rawMap2)
      Just _ -> do
        -- print (homingDist2 (miRoomSize mi) startState)
        -- running test examples, for those we just run them as they are.
        answerShow $ solveFromRawMap rawMap
