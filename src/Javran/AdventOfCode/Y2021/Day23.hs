{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2021.Day23
  (
  )
where

import Control.Applicative
import Control.Lens hiding (universe)
import Control.Monad
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import qualified Data.PSQueue as PQ
import Data.Semigroup
import qualified Data.Set as S
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
  Side note: attempted to switch to Data.OrdPSQ from "psqueues" package,
  but the performance is actually worse.
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

type CoordSet = [Coord] -- must be sorted and unique list

useAmpGraph :: Bool
useAmpGraph = True

{-
  The factor that the entire room is completely determined
  by room size allows us to compute the graph on the fly
  rather than doing lookup on the actual data.
 -}
ampGraph :: Int -> Coord -> [Coord]
ampGraph roomSize (r, c)
  | r == 1 && (c >= 1 && c <= 11) =
    [(r, c -1) | c > 1] <> [(r, c + 1) | c < 11]
      <> [(r + 1, c) | isRoomCol]
  | r >= 2 && (r <= roomSize + 1) && isRoomCol =
    (r -1, c) : [(r + 1, c) | r < roomSize + 1]
  | otherwise = []
  where
    isRoomCol = c `elem` [3, 5, 7, 9]

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
      unless (all ((== miRoomSize) . length) ampLocs) do
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
      pure (coord, [coord'])
    (graphPre, ampLocsPre) = unzip do
      (r, rs) <- zip [0 ..] raw
      (c, x) <- zip [0 ..] rs
      guard $ x `notElem` "# "
      let coord = (r, c)
          mAmp = case reads @AmpType [x] of
            [(ampTy, "")] -> Just ampTy
            _ -> Nothing
      pure (coord, (\amp -> M.singleton amp [coord]) <$> mAmp)

{-
  The space immediately outside any room,
  This is consistent across maps so that we can hard-code.
 -}
isCan'tStopCoord :: Coord -> Bool
isCan'tStopCoord (r, c) = r == 1 && c `elem` [3, 5, 7, 9]

isInHallway :: Coord -> Bool
isInHallway (r, _) = r == 1

moveCost :: AmpType -> Int
moveCost = \case
  A -> 1
  B -> 10
  C -> 100
  D -> 1000

homeColumn :: AmpType -> Int
homeColumn = \case
  A -> 3
  B -> 5
  C -> 7
  D -> 9

moveTargets :: Int -> AmpType -> [Coord]
moveTargets roomSize ampType = fmap (,homeColumn ampType) rs
  where
    rs = take roomSize [2 ..]

_pprWorldState :: Int -> WorldState -> String -> IO ()
_pprWorldState roomSize ws pad = do
  let ampLocs :: M.Map Coord AmpType
      ampLocs = M.fromList do
        (aTy, cs) <- zip [A .. D] ws
        coord <- cs
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
    putStrLn $ pad <> concatMap render [0 .. length (head raw) -1]

manhattan :: Coord -> Coord -> Int
manhattan (a, b) (c, d) = abs (a - c) + abs (b - d)

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
ampHomingDists roomSize ampType cs =
  sum (fmap snd tracedDists)
    + alreadyHomeIncr
    + stillOutsideIncr
  where
    tracedDists = fmap (\c -> (c, ampHomingDist ampType c)) cs
    (alreadyHome, stillOutside) = partition ((== 0) . snd) tracedDists
    alreadyHomeSortedRowsDesc =
      sortOn Down $
        fmap (\((r, _c), _) -> r) alreadyHome
    {-
      room row range: [2 .. roomSize-1]
      compute how many more most to "pack" those already home to bottom.
     -}
    alreadyHomeIncr =
      sum $
        zipWith (-) [roomSize + 1, roomSize ..] alreadyHomeSortedRowsDesc
    stillOutsideIncr = cnt * (cnt -1) `quot` 2
      where
        cnt = length stillOutside

{-

  Computes an underestimation of energy cost from current state to goal state.

  This estimation is done by moving all amps to their target locations,
  through open spaces assuming there is nothing in-between target location and the amp.

  It should be the case that:
  - homingEnergy ws == 0 <==> ws is goal state
  - homingEnergy ws >= 0

 -}
homingEnergy :: Int -> WorldState -> Int
homingEnergy roomSize ws =
  sum $
    zipWith
      (\ampType cs ->
         moveCost ampType * ampHomingDists roomSize ampType cs)
      [A .. D]
      ws

impliesM :: Alternative f => Bool -> Bool -> f ()
p `impliesM` q = guard $ not p || q

findNextMoves :: MapInfo -> AmpType -> Coord -> WorldState -> [(Coord, WorldState, Int)]
findNextMoves MapInfo {miRoomSize, miGraph} ampType initCoord wsPre =
  findNextMovesAux (PQ.singleton initCoord 0) (S.singleton initCoord)
  where
    ampLocs :: [(Coord, AmpType)]
    ampLocs = do
      (aTy, cs) <- zip [A .. D] ws
      coord <- cs
      pure (coord, aTy)

    ws = wsPre & ix (fromEnum ampType) %~ delete initCoord

    blockings :: [Coord]
    blockings = concat ws

    getNextsOf coord =
      if useAmpGraph
        then ampGraph miRoomSize coord
        else do
          Just coords' <- [miGraph M.!? coord]
          coords'

    myMoveTargets = moveTargets miRoomSize ampType
    myHomeCol = homeColumn ampType
    targetRoomIsClear =
      {-
        a clear room is only occupied by the same ampType,
        open space allowed.
       -}
      all
        (\targetCoord -> case lookup targetCoord ampLocs of
           Nothing -> True
           Just ampType' -> ampType' == ampType)
        myMoveTargets

    findNextMovesAux q0 discovered = case PQ.minView q0 of
      Nothing -> []
      Just (coord@(r, c) PQ.:-> energy, q1) ->
        [ (coord, ws & ix (fromEnum ampType) %~ insert coord, energy)
        | not (isCan'tStopCoord coord)
        ]
          <> let nexts = do
                   coord'@(r', c') <- getNextsOf coord
                   guard $ coord' `notElem` blockings && S.notMember coord' discovered
                   (isInHallway coord && not (isInHallway coord'))
                     `impliesM` (
                                 -- when moving down from hallway.
                                 c' == myHomeCol
                                   &&
                                   -- also room must be clear
                                   targetRoomIsClear)
                   (r > 1 && c == myHomeCol && targetRoomIsClear)
                     `impliesM`
                     -- if we are already in target room and that room is clear
                     -- it won't do us much good moving up again
                     (r' > r)
                   (not (isInHallway coord) && c /= myHomeCol)
                     `impliesM`
                     -- must move up
                     (r' < r)
                   pure coord'
                 q2 = foldr upd q1 nexts
                   where
                     upd coord' = PQ.insert coord' $! energy + moveCost ampType
                 discovered' = foldr S.insert discovered nexts
              in findNextMovesAux q2 discovered'

{-
  https://en.wikipedia.org/wiki/A*_search_algorithm
 -}
aStar :: MapInfo -> PQ.PSQ WorldState (Arg Int Int) -> HM.HashMap WorldState Int -> Int
aStar mi@MapInfo {miRoomSize} q0 gScores = case PQ.minView q0 of
  Nothing -> error "queue exhausted"
  Just (ws PQ.:-> (Arg fScore energy), q1) ->
    if fScore == energy
      then energy
      else
        let gScore = gScores HM.! ws
            nexts = do
              (ampType, coords) <- zip [A .. D] ws
              coord@(_r, c) <- coords
              let myHomeCol = homeColumn ampType
              ((r', c'), ws', incr) <- findNextMoves mi ampType coord ws
              -- if in hallway, must move to a room
              isInHallway coord
                `impliesM` let targetIsHomeCol = c' == myHomeCol
                               enforceSqueeze =
                                 -- and squeeze it down all the way.
                                 r' == miRoomSize + 1
                                   || (r' + 1, c') `elem` (ws' !! fromEnum ampType)
                            in targetIsHomeCol && enforceSqueeze

              (not (isInHallway coord) && c /= myHomeCol)
                `impliesM`
                -- if in the wrong room, we must move out.
                -- (moving up one place may be possible, just not very productive)
                (r' == 1 || c' == myHomeCol)
              let gScore' = gScore + incr
                  fScore' = gScore' + homingEnergy miRoomSize ws'
                  energy' = energy + incr
              guard $ case gScores HM.!? ws' of
                Nothing -> True
                Just v -> gScore' < v
              pure (ws', gScore', Arg fScore' energy')
            q2 = foldr upd q1 nexts
              where
                upd (ws', _, prio') = PQ.insert ws' prio'
            gScores' = foldr upd gScores nexts
              where
                upd (ws', gScore', _) = HM.insert ws' gScore'
         in aStar mi q2 gScores'

solveFromRawMap :: [String] -> Int
solveFromRawMap rawMap =
  aStar
    mi
    (PQ.singleton
       startState
       (Arg initHoming 0))
    (HM.singleton startState 0)
  where
    (mi, startState) = parseRawMap rawMap
    initHoming = homingEnergy (miRoomSize mi) startState

instance Solution Day23 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (extraOps, rawInput) <- consumeExtraLeadingLines <$> getInputS
    let rawMap = lines rawInput
    case extraOps of
      Nothing -> do
        -- running login input.
        answerShow $! solveFromRawMap rawMap
        let rawMap2 =
              let (xs, ys) = splitAt 3 rawMap
               in xs
                    <> [ "  #D#C#B#A#"
                       , "  #D#B#A#C#"
                       ]
                    <> ys
        answerShow $! solveFromRawMap rawMap2
      Just _ ->
        -- test input runs as-is.
        answerShow $! solveFromRawMap rawMap
