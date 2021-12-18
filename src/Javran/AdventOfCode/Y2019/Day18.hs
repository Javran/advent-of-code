{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2019.Day18
  (
  )
where

import Control.Lens
import Control.Monad
import qualified Data.Array as Arr
import qualified Data.Array.IArray as IArr
import Data.Char
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.PSQueue as PQ
import qualified Data.Set as S
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude

data Day18 deriving (Generic)

{-
  Notes:

  Step #1: simplification.

  Looks like all those maps have some properties that we can take advantage of:

  - there very little presence of 2x2 open cells in the entire map, meaning that there is probably
    only handful of ways (I'll bet 1 or 2 without even looking into details)
    to go from one place to another.

  - if we only look at empty cells,
    there are many tunnels (degree of 2) and dead ends (degree of 1), that
    we can probably get rid of while keeping necessary info of the map intact.

  So instead of performing some search on the input map, we can condense it
  into a graph whose nodes are map cells and edges encode distances between nodes.

  After this simplification we can "jump" between important cells and avoid ever
  touching part of the map that does not do anything.

  Step #2:

  Seaching with priority queue should do to complete the puzzle.

  Note that (<current coord>, <step count>, <missing keys>) fully describes a search state,
  we can be smart about what to be keys and what to be priorities in the queue.

  My choice:

  - priority: (<step count>, <# of missing keys>), having the second part allows
    us to visit states that have more keys collected first.

  - key: (<current cord>, <missing keys>), step length can be extracted from priority.

  Step #3:

  If you are patient, step #2 is good enough for both parts of the puzzle, but
  the algorithm can be further sped up.

  The idea is to make sure a productive branching strategy. A robot that simply
  jumps between cells is not helpful, but when it goes down a path that would result
  in collecting a key, that's progress. Simply having this check after branching
  is able to pruning a fairly large amount of unnecessary branching.

 -}

data Cell
  = COpen
  | CEntrance
  | CWall
  | CKey Int -- [0..25] for letters
  | CDoor Int -- [0..25] for letters
  deriving (Eq)

instance Show Cell where
  show =
    (: []) . \case
      COpen -> '.'
      CEntrance -> '@'
      CWall -> '#'
      CKey v -> chr (v + ord 'a')
      CDoor v -> chr (v + ord 'A')

parseCell :: Char -> Cell
parseCell = \case
  '.' -> COpen
  '@' -> CEntrance
  '#' -> CWall
  ch ->
    if
        | isAsciiLower ch -> CKey (ord ch - ord 'a')
        | isAsciiUpper ch -> CDoor (ord ch - ord 'A')
        | otherwise -> errInvalid

type Coord = (Int, Int) -- row and col

data MapInfo = MapInfo
  { miGet :: Coord -> Maybe Cell
  , miGraph :: M.Map Coord (S.Set Coord)
  , -- | distance between two coords (c,c'), where c < c'
    -- since the graph is bidirectional, we only need to store half.
    miDist :: M.Map (Coord, Coord) Int
  , miAllKeys :: IS.IntSet
  }

getDist :: M.Map (Coord, Coord) Int -> (Coord, Coord) -> Maybe Int
getDist m (a, b) = m M.!? if a < b then (a, b) else (b, a)

safeIndexArr :: (Arr.Ix i, IArr.IArray a e) => a i e -> i -> Maybe e
safeIndexArr arr i =
  arr IArr.! i
    <$ guard (inRange (IArr.bounds arr) i)

mkMapInfo :: IArr.Array (Int, Int) Cell -> MapInfo
mkMapInfo floorPlan = MapInfo {miGraph, miDist, miGet = safeIndexArr floorPlan, miAllKeys}
  where
    miGraph = M.unionsWith (<>) gPre
    miDist = M.unionsWith (+) dPre
    miAllKeys = IS.fromList do
      CKey k <- IArr.elems floorPlan
      pure k
    (gPre, dPre) = unzip do
      (c, x) <- Arr.assocs floorPlan
      guard $ x /= CWall
      c' <- udlrOfCoord c
      Just x' <- pure $ safeIndexArr floorPlan c'
      guard $ x' /= CWall
      pure
        ( M.singleton c (S.singleton c')
        , if c < c' then M.singleton (c, c') (1 :: Int) else M.empty
        )

simplifyMapInfo :: MapInfo -> MapInfo
simplifyMapInfo mi@MapInfo {miGraph, miGet} = simplifyMapInfoAux mi $ PQ.fromList do
  coord <- M.keys miGraph
  Just COpen <- pure (miGet coord)
  pure (coord PQ.:-> S.size (miGraph M.! coord))

{-
  For the priority queue, the invariant is that only COpen cells are allowed to be in the queue.
 -}
simplifyMapInfoAux :: MapInfo -> PQ.PSQ Coord Int -> MapInfo
simplifyMapInfoAux mi@MapInfo {miGraph, miGet, miDist} q0 = case PQ.minView q0 of
  Nothing -> mi
  Just (c PQ.:-> deg, q1) -> case deg of
    1 ->
      let [c'] = S.toList (miGraph M.! c)
          miGraph' = M.adjust (S.delete c) c' $ M.delete c miGraph
          q2 =
            if miGet c' == Just COpen
              then PQ.insert c' (S.size $ miGraph' M.! c') q1
              else q1
       in simplifyMapInfoAux mi {miGraph = miGraph'} q2
    2 ->
      let [c1, c2] = S.toList (miGraph M.! c)
          miGraph' =
            {-
              note that in this process we could create a node that links to itself,
              if the original graph contains one.
              but that will get eliminated in a subsequent simplification (by degree 1 case).
             -}
            M.adjust (S.insert c1 . S.delete c) c2 $
              M.adjust (S.insert c2 . S.delete c) c1 $
                M.delete c miGraph
          miDist' =
            let p = if c1 < c2 then (c1, c2) else (c2, c1)
                newDist = fromJust (getDist miDist (c, c1)) + fromJust (getDist miDist (c, c2))
             in -- probably not worth removing old ones
                M.insert p newDist miDist
          enqueue cx =
            if miGet cx == Just COpen
              then PQ.insert cx (S.size $ miGraph' M.! cx)
              else id
          q2 = enqueue c1 . enqueue c2 $ q1
       in simplifyMapInfoAux mi {miGraph = miGraph', miDist = miDist'} q2
    _
      | deg >= 3 ->
        -- meaning all deg 1 and 2 are done.
        mi
    _ -> unreachable

type MissingKeys = IS.IntSet

type SearchKey = ([Coord], MissingKeys)

type SearchQueue =
  PQ.PSQ
    SearchKey
    ( Int -- # of steps taken
    , Int -- size of missing keys.
    )

debugMapInfo :: Int -> Int -> MapInfo -> IO ()
debugMapInfo rows cols mi = do
  forM_ [0 .. rows -1] $ \r -> do
    let render c = case miGet mi coord of
          Nothing -> unreachable
          Just cell -> case cell of
            COpen -> if isJust v then show $ fromJust (miGet mi coord) else " "
            _ -> show $ fromJust (miGet mi coord)
          where
            v = miGraph mi M.!? coord
            coord = (r, c)
    putStrLn (concatMap render [0 .. cols -1])
  c <- forM (M.toAscList (miDist mi)) $ \((c0, c1), dist) -> do
    let v0 = fromJust (miGet mi c0)
        v1 = fromJust (miGet mi c1)
    if M.member c0 (miGraph mi) && M.member c1 (miGraph mi)
      then do
        putStrLn $
          show c0
            <> " '"
            <> show v0
            <> "' <=> "
            <> show c1
            <> " '"
            <> show v1
            <> "': "
            <> show dist
        pure (1 :: Int)
      else pure 0
  print $ sum c

type FloorPlan = Arr.Array Coord Cell

updateFloorPlanForPart2 :: FloorPlan -> FloorPlan
updateFloorPlanForPart2 fp =
  fp Arr.// do
    let coords =
          (,)
            <$> [r0 -1 .. r0 + 1]
            <*> [c0 -1 .. c0 + 1]
        rawUpdate =
          fmap
            parseCell
            "@#@\
            \###\
            \@#@"
    zip coords rawUpdate
  where
    (r0, c0) = head do
      (coord, CEntrance) <- IArr.assocs fp
      pure coord

{-
  TODO: we can probably use this for branching rather than
  using this for pruning after branching.

  Note: one needs to be careful when extending this to handle branching -
  if we jump aggressively, chances are we end up with extra steps that
  are not possible to get rid of.

 -}
findReachableKeys :: MapInfo -> IS.IntSet -> Coord -> S.Set Coord -> [()]
findReachableKeys mi@MapInfo {miGraph, miGet} missingKeys coord visited = do
  let proceed = do
        coord' <- S.toList (miGraph M.! coord)
        guard $ S.notMember coord' visited
        findReachableKeys mi missingKeys coord' (S.insert coord' visited)
  case fromJust (miGet coord) of
    COpen -> proceed
    CEntrance -> proceed
    CWall -> unreachable
    CKey k ->
      if IS.member k missingKeys
        then pure ()
        else proceed
    CDoor k -> guard (IS.notMember k missingKeys) *> proceed

bfs
  :: MapInfo
  -> SearchQueue
  -> S.Set SearchKey
  -> Int
bfs mi@MapInfo {miGraph, miGet, miDist} q0 discovered =
  case PQ.minView q0 of
    Nothing -> error "queue exhausted"
    Just ((coords, missingKeys) PQ.:-> (stepCount, missingKeyCount), q1) ->
      if missingKeyCount == 0
        then stepCount
        else
          let nexts = do
                (i, coord) <- zip [0 :: Int ..] coords
                coord' <- S.toList (miGraph M.! coord)
                -- only allow this move when it leads to collecting some new keys.
                _ : _ <-
                  pure $
                    findReachableKeys mi missingKeys coord' (S.fromList [coord, coord'])
                let stepCount' = stepCount + fromJust (getDist miDist (coord, coord'))
                    coords' = coords & ix i .~ coord'
                missingKeys' <-
                  let ok = pure missingKeys
                   in case fromJust (miGet coord') of
                        COpen -> ok
                        CEntrance -> ok
                        CWall -> unreachable
                        CKey k -> pure (IS.delete k missingKeys)
                        CDoor k -> guard (IS.notMember k missingKeys) *> ok
                guard $ S.notMember (coords', missingKeys') discovered
                pure ((coords', missingKeys'), stepCount')
              (discovered', q2) = foldr performUpdate (discovered, q1) nexts
                where
                  performUpdate (k@(_, missingKeys'), stepCount') (curDiscovered, curQ) =
                    ( S.insert k curDiscovered
                    , PQ.alter
                        (let p' = (stepCount', IS.size missingKeys')
                          in \case
                               Nothing -> Just p'
                               Just p -> Just (min p' p))
                        k
                        curQ
                    )
           in q2 `seq` discovered' `seq` bfs mi q2 discovered'

startBfs :: MapInfo -> Int
startBfs mi@MapInfo {miGet, miGraph, miAllKeys} =
  bfs
    mi
    (PQ.singleton startKey (0, IS.size miAllKeys))
    (S.singleton startKey)
  where
    startKey :: SearchKey
    startKey = (coordEnts, miAllKeys)
    coordEnts = do
      coord <- M.keys miGraph
      Just CEntrance <- pure (miGet coord)
      pure coord

instance Solution Day18 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (extraOps, rawInput) <- consumeExtraLeadingLines <$> getInputS
    let xs = lines rawInput
        rows = length xs
        cols = length (head xs)
        runPart1 = maybe True ("part1" `elem`) extraOps
        runPart2 = maybe True ("part2" `elem`) extraOps
        floorPlan :: FloorPlan
        floorPlan = Arr.array
          ((0, 0), (rows -1, cols -1))
          do
            (r, rs) <- zip [0 ..] xs
            (c, x) <- zip [0 ..] rs
            pure ((r, c), parseCell x)
        debug = False
    when runPart1 do
      let mi = simplifyMapInfo $ mkMapInfo floorPlan
      when debug $ debugMapInfo rows cols mi
      answerShow $ startBfs mi
    when runPart2 do
      let floorPlan2 = updateFloorPlanForPart2 floorPlan
          mi = simplifyMapInfo $ mkMapInfo floorPlan2
      when debug $ debugMapInfo rows cols mi
      answerShow $ startBfs mi
