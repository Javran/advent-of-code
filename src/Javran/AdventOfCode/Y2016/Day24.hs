{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2016.Day24
  (
  )
where

import Control.Monad
import qualified Data.Array as Arr
import Data.Char
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.PSQueue as PQ
import qualified Data.Set as S
import Javran.AdventOfCode.GridSystem.RowThenCol.Uldr
import Javran.AdventOfCode.Prelude

data Day24 deriving (Generic)

data MapInfo = MapInfo
  { -- | the coord tagged `0`
    miStart :: Coord
  , miGraph :: M.Map Coord [Coord]
  , -- | distance between coords,
    --   might contain edges that no longer exist due to simplification
    miDist :: M.Map (MinMax Coord) Int
  , -- | all number locations
    miNums :: M.Map Coord Int
  , -- | rows and cols
    miDims :: (Int, Int)
  }
  deriving (Show)

parseFromRaw :: [String] -> MapInfo
parseFromRaw xs = MapInfo {miGraph, miDist, miStart, miNums, miDims}
  where
    rows = length xs
    cols = length (head xs)
    miDims = (rows, cols)
    bds = ((0, 0), (rows -1, cols -1))
    rawMap = Arr.array bds do
      (r, rs) <- zip [0 ..] xs
      (c, x) <- zip [0 ..] rs
      pure ((r, c), x)
    gPre = do
      r <- [0 .. rows -1]
      c <- [0 .. cols -1]
      let coord = (r, c)
          ch = rawMap Arr.! coord
      guard $ ch /= '#'
      let vs = do
            coord' <- uldrOfCoord coord
            guard $ inRange bds coord'
            guard $ rawMap Arr.! coord' /= '#'
            pure coord'
      pure (coord, ch, vs)
    miGraph = M.fromList $ fmap (\(c, _, vs) -> (c, vs)) gPre
    miDist = M.fromList $ do
      (u, _, vs) <- gPre
      v <- vs
      guard $ u <= v
      pure (minMaxFromPair (u, v), 1)
    nums@((0, miStart) : _) = sortOn fst do
      (u, ch, _) <- gPre
      guard $ isDigit ch
      pure (ord ch - ord '0', u)
    miNums = M.fromList $ fmap swap nums

getDist :: M.Map (MinMax Coord) Int -> (Coord, Coord) -> Maybe Int
getDist m p = m M.!? minMaxFromPair p

{-
  Simplifies a graph by pruning "boring" nodes,
  while preserving graph's shortest paths between tagged nodes.

  A "boring" node is a node without tag that meets one of the following requirement:

  - of degree 0 or 1: a "dead end" node that can simply be removed from graph.
  - of degree 2: a "tunnel" node that connects exactly 2 other nodes
    (one needs to be careful that removal of this node should not destroy the shortest
    path passing it)

  This is very similar to what we did in Y2019 Day 18 and 20, the simplification
  allows us to travel between nodes with less intermediate hops.

  Note that this is not a necessary step, but it could speed up subsequent
  shortest-path finding as we have way less nodes to consider
  (due to the "tunnel-rich" nature of input maze)
 -}
simplifyMapInfo :: MapInfo -> MapInfo
simplifyMapInfo miIn = simp miIn $ PQ.fromList do
  (coord, cs) <- M.toList (miGraph miIn)
  let deg = length cs
  guard $ deg <= 2
  pure (coord PQ.:-> deg)
  where
    simp :: MapInfo -> PQ.PSQ Coord Int -> MapInfo
    simp mi@MapInfo {miGraph, miDist, miNums} q0 = case PQ.minView q0 of
      Nothing -> mi
      Just (c PQ.:-> deg, q1) ->
        if M.member c miNums
          then simp mi q1
          else case deg of
            0 ->
              let miGraph' = M.delete c miGraph
               in simp mi {miGraph = miGraph'} q1
            1 ->
              let [c'] = miGraph M.! c
                  miGraph' = M.adjust (delete c) c' $ M.delete c miGraph
                  q2 = PQ.insert c' (length $ miGraph' M.! c') q1
               in simp mi {miGraph = miGraph'} q2
            2 ->
              let [c1, c2] = miGraph M.! c
                  mOldDist = getDist miDist (c1, c2)
                  newDist = fromJust (getDist miDist (c, c1)) + fromJust (getDist miDist (c, c2))
                  safeToPrune = case mOldDist of
                    Nothing -> True
                    Just oldDist -> newDist <= oldDist
                  miGraph' =
                    M.adjust ((c1 :) . delete c) c2 $
                      M.adjust ((c2 :) . delete c) c1 $
                        M.delete c miGraph
                  miDist' =
                    -- probably not worth removing old ones
                    M.insert (minMaxFromPair (c1, c2)) newDist miDist
                  enqueue cx = PQ.insert cx (length $ miGraph' M.! cx)
                  q2 = enqueue c1 . enqueue c2 $ q1
               in if safeToPrune
                    then simp mi {miGraph = miGraph', miDist = miDist'} q2
                    else simp mi q1
            _
              | deg >= 3 ->
                -- meaning all deg 1 and 2 are done.
                mi
            _ -> unreachable

{-
  Finds single-source shortest direct distances to `target` nodes.

  "Direct" means that we don't consider paths that "go beyond" target nodes.
  In some cases (especially puzzle's example) this allows terminating
  shortest path finding earlier - it's unnecessary to find paths beyond direct reach
  because passing through a node without actually visiting it can't be the optimal solution.

 -}
shortestDirectDists :: MapInfo -> S.Set Coord -> M.Map Coord Int -> PQ.PSQ Coord Int -> M.Map Coord Int
shortestDirectDists MapInfo {miGraph, miDist} targets = fix \go dists q0 -> case PQ.minView q0 of
  Nothing -> M.restrictKeys dists targets
  Just (u PQ.:-> distU, q1) ->
    let nexts = do
          -- not allowed to expand further if we have reached some targets.
          guard $ S.notMember u targets
          Just vs <- pure (miGraph M.!? u)
          v <- vs

          let distV' = distU + fromJust (getDist miDist (u, v))
              mDistV = dists M.!? v
          guard $ maybe True (distV' <) mDistV
          pure (v, distV')
        dists' = foldr (\(v, distV') -> M.insert v distV') dists nexts
        q2 = foldr (\(v, distV') -> PQ.insert v distV') q1 nexts
     in go dists' q2

type SearchState =
  ( Int -- current node
  , IS.IntSet -- nodes not yet visited
  )

salesman :: IM.IntMap [(Int, Int)] -> (SearchState -> Bool) -> M.Map SearchState Int -> PQ.PSQ SearchState Int -> Int
salesman simpleDists isDone = fix \go pathLens q0 -> case PQ.minView q0 of
  Nothing -> error "queue exhausted"
  Just (ss@(u, todos) PQ.:-> len, q1) ->
    if isDone ss
      then len
      else
        let nexts = do
              Just vs <- pure $ simpleDists IM.!? u
              (v, d) <- vs
              let todos' = IS.delete v todos
                  len' = len + d
                  next = (v, todos')
              guard $ maybe True (len' <) $ pathLens M.!? next
              pure (next, len')
            pathLens' = foldr (\(next, len') -> M.insert next len') pathLens nexts
            q2 = foldr (\(next, prio) -> PQ.insert next prio) q1 nexts
         in go pathLens' q2

{-
  The key insight of this puzzle is that, despite having many nodes in the graph,
  we have only n <= 10 nodes that we are interested in.

  Therefore, if we can simplify the graph to the point that we are only taking
  about shortest paths between those n nodes, we have a very small search space to deal with.
 -}
instance Solution Day24 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    miPre <- parseFromRaw . lines <$> getInputS
    let mi = simplifyMapInfo miPre
        points = M.keysSet $ miNums mi
        ptToInt = (miNums mi M.!)
        simpleDists :: IM.IntMap [(Int, Int)]
        simpleDists = IM.fromList do
          {-
            re-constructs the graph and preseves only those tagged nodes
            distances between tagged nodes are shortest distances between them.
           -}
          src <- S.toList points
          let targets = S.delete src points
              dists = shortestDirectDists mi targets M.empty (PQ.singleton src 0)
          pure (ptToInt src, (fmap . first) ptToInt $ M.toList dists)
        initSt = (0, initTodos)
        initTodos = IS.delete 0 $ IS.fromList $ M.elems $ miNums mi
        solveSalesman isDone =
          salesman
            simpleDists
            isDone
            (M.singleton initSt 0)
            (PQ.singleton initSt 0)
    answerShow $ solveSalesman (IS.null . snd)
    answerShow $ solveSalesman (\(cur, todos) -> IS.null todos && cur == 0)
