{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2021.Day23
  (
  )
where

import Control.Lens hiding (universe)
import Control.Monad
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord
import qualified Data.PSQueue as PQ
import qualified Data.Set as S
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude

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

_pprWorldState :: WorldState -> IO ()
_pprWorldState ws = do
  let ampLocs :: M.Map Coord AmpType
      ampLocs = M.fromList do
        (aTy, cs) <- zip [A .. D] ws
        coord <- S.toList cs
        pure (coord, aTy)
      raw =
        [ "#############"
        , "#..!.!.!.!..#"
        , "###.#.#.#.###"
        , "  #.#.#.#.#  "
        , "  #.#.#.#.#  "
        , "  #.#.#.#.#  "
        , "  #########  "
        ]
  forM_ [0 .. length raw -1] $ \r -> do
    let render c
          | Just ampTy <- ampLocs M.!? (r, c) =
            show ampTy
          | otherwise = [raw !! r !! c]
    putStrLn $ concatMap render [0 .. length (head raw) -1]

targetWorld :: Int -> WorldState
targetWorld roomSize = fmap (moveTargets roomSize) [A .. D]

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
      Just (coord PQ.:-> energy, q1) ->
        [ (coord, ws & ix (fromEnum ampType) %~ S.insert coord, energy)
        | S.notMember coord can'tStops
        ]
          <> let nexts = do
                   Just coords' <- [miGraph M.!? coord]
                   coord'@(r', _) <- S.toList coords'
                   when (isInHallway coord && 2 <= r' && r' <= 5) do
                     let myMoveTargets = moveTargets miRoomSize ampType
                     guard $ S.member coord' myMoveTargets
                     -- also room must be clear
                     forM_ myMoveTargets \tgtCoord ->
                       guard $ case ampLocs M.!? tgtCoord of
                         Nothing -> True
                         Just ampType' -> ampType' == ampType
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
              in findNextMovesAux q2 (S.union discovered (S.fromList nexts))

type SearchPrio =
  ( Down Int
  , Int -- energy
  )

bfs :: MapInfo -> PQ.PSQ WorldState SearchPrio -> S.Set WorldState -> Int
bfs mi@MapInfo {miRoomSize} q0 discovered = case PQ.minView q0 of
  Nothing -> error "queue exhausted"
  Just (ws PQ.:-> (_hp, energy), q1) ->
    if ws == wsTarget
      then energy
      else
        let nexts = do
              (ampType, coords) <- zip [A .. D] ws
              coord <- S.toList coords
              let curMoveTargets = moveTargets miRoomSize ampType
              (coord', ws', incr) <- findNextMoves mi ampType coord ws
              -- if in hallway, must move to a room
              when (isInHallway coord) $
                guard $ S.member coord' curMoveTargets
              guard $ S.notMember ws' discovered
              pure (ws', incr)
            q2 = foldr upd q1 nexts
              where
                upd (ws', incr) curQ =
                  PQ.alter
                    (\case
                       Nothing -> Just (hp, energy')
                       Just (_, e) -> Just (hp, min e energy'))
                    ws'
                    curQ
                  where
                    hp = homingPriority ws' wsTarget
                    energy' = energy + incr
         in bfs mi q2 (S.union discovered (S.fromList $ fmap fst nexts))
  where
    wsTarget = targetWorld miRoomSize

instance Solution Day23 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- lines <$> getInputS
    let (mi, startState) = parseRawMap xs
        targetState = targetWorld (miRoomSize mi)
    answerShow $
      bfs mi (PQ.singleton startState (homingPriority startState targetState, 0)) (S.singleton startState)