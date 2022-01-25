{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2016.Day22
  (
  )
where

import Control.Monad
import Data.List.Ordered (nubSort)
import qualified Data.Map.Strict as M
import qualified Data.PSQueue as PQ
import Data.Semigroup
import qualified Data.Set as S
import Javran.AdventOfCode.GridSystem.RowThenCol.Uldr
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day22 deriving (Generic)

data Node = Node
  { nSize :: Int
  , nUsed :: Int
  , nAvail :: Int
  , nUsePercent :: Int
  }
  deriving (Show)

nodeP :: ReadP (Coord, Node)
nodeP = do
  strP "/dev/grid/node-x"
  x <- decimal1P
  strP "-y"
  y <- decimal1P <* skipSpaces
  nSize <- decimal1P <* charP 'T' <* skipSpaces
  nUsed <- decimal1P <* charP 'T' <* skipSpaces
  nAvail <- decimal1P <* charP 'T' <* skipSpaces
  nUsePercent <- decimal1P <* charP '%'
  {-
    Coord order is y-then-x,
    so that it's consistent with row-then-col grid system.
   -}
  pure ((y, x), Node {nSize, nUsed, nAvail, nUsePercent})

{-
  Gets the bound of the grid (rMax, cMax),
  and verifies that all coords within (inclusively) (0,0) ~ (rMax, cMax)
  are present and correct.
 -}
checkCompleteness :: [Coord] -> Maybe (Int, Int)
checkCompleteness xs = do
  (allCoords, Just (MinMax2D ((0, rMax), (0, cMax)))) <-
    pure $ foldMap (\c -> (S.singleton c, Just $ minMax2D c)) xs
  guard $
    allCoords == S.fromDistinctAscList do
      (,) <$> [0 .. rMax] <*> [0 .. cMax]
  pure (rMax, cMax)

{-
  Notes regarding solving part 2:

  For this part, we'd like to make following assumptions:

  #1 Only one "empty" node that is capable of "moving around".

    To "move around" the empty node is to move adjacent data into it
    while the destination has enough capacity.

  #2 No "data merge" could happen.

    That is, we don't consider cases where merges two non-empty data,
    as this will result in more than one empty block that we can work with.

  #3 The whole grid forms a rectangle, and have all nodes available inside of it.

    This is not a necessary condition for solving this puzzle, but so far all
    login data has this property, and this makes it a bit easier to visualize and debug.

    This assumption is verified by `checkCompleteness` function, which also gives
    the grid's dimension.

  #4 For actual login input (rather than examples),
    all nodes fall into one of the following 3 categories (ranges are all inclusive):

    C1: Used: 0, Size in [85..94]
    C2: Used in [64..73], Size in [85..94]
    C3: Used in [490..499], Size in [501..510]

    This assumption is made after examining my login input
    (and some other input data obtained after solving this puzzle)
    Also note that having this assumption also confirms assumption #2:
    even with two smallest C2 node, 64+64 < 94, which can't fit into any C2 node.
    Although C3 nodes have enough capacity, they are basically "walls" as empty node
    will never have sufficient capacity to receive their contents.

    Assumption #4 allows a crucial normalization that
    can narrow down search space quite a bit, in short:

    - C3 are "walls"
    - the empty cell can move freely between C1 and C2 nodes,
      without worrying about capacity.

    Since there's no point "shuffling C2 nodes around to make rooms for moving the target",
    we might as well treat all C2 nodes as identical, this gives the following normalization:

    - Treat the C1 node (only one) as Used: 0, Size: 80
    - Treat C2 nodes as Used: 70, Size: 80
    - Treat C3 nodes as Used: 490, Size: 500

    Numbers are arbitrarily chosen around original value ranges - we just need some
    value that maintains desired properties.

    This normalization eliminates useless shuffling, as different shuffles results
    in identical search state.

 -}

{-
  This heuristic is based on the fact that for every move that
  brings goal block closer to the origin (except for the last step
  the brings goal block to the origin)
  it is accompanied by shuffling the empty block around so that
  we can advance the goal block again.

  Let [g] be goal block, [e] empty block, [x] an occupied block:

  0: [x] [e]-[g]

  1: [x] [g] [e]
              |
             [x]

  2: [x] [g] [x]

         [x]-[e]

  3: [x] [g] [x]

     [x]-[e] [x]

  4: [x] [g] [x]
      |
     [e] [x] [x]

  5: [e] [g] [x]

     [x] [x] [x]

  Therefore, if we need `dist` steps moving goal block to the origin
  pretending all other blocks are empty, we need `dist` moves.
  If dist > 0, it is also accompanied by `4*(dist-1)` extra moves
  for shuffling the empty block around.

  dist + 4 * (dist - 1) (dist > 0)
  ==> 5 * dist - 4

  Note that so far we haven't take care of where the empty block is,
  but it's obvious that we need to bring the block closer to the goal
  block to make any progress, I'm making a random guess that
  if the dist between empty block and target block is > 2
  (as the empty block shuffling process never brings empty block
  further than 2 to the goal block), we include an extra distance of:

  `dist(empty, goal) - 2`

  in our heuristic.
 -}
estimateDist :: Coord -> Coord -> Int
estimateDist c e = if dist == 0 then 0 else 5 * dist - 4 + extra
  where
    distCe = manhattan c e
    extra = if dist > 0 && distCe > 2 then distCe - 2 else 0
    dist = manhattan (0, 0) c

type SearchState =
  ( Coord -- location of the empty block
  , Coord -- location of the goal block, and all current nodes.
  , M.Map Coord Int -- current used fields.
  )

aStar :: M.Map Coord Int -> M.Map SearchState Int -> PQ.PSQ SearchState (Arg Int Int) -> Int
aStar capacities gScores q0 = case PQ.minView q0 of
  Nothing -> error "queue exhausted"
  Just ((coord, targetNode, ns) PQ.:-> (Arg fScore gScore), q1) ->
    if fScore == gScore
      then gScore
      else
        let nexts = do
              let used = 0
                  capa =
                    if ns M.! coord == 0
                      then capacities M.! coord
                      else error "empty grid is not empty?"
              coord' <- uldrOfCoord coord
              {-
                Clarification: we are "moving" the empty block
                from coord to coord', which means, in operational terms,
                moving data from coord' to coord.
               -}
              Just used' <- pure (ns M.!? coord')
              let newUsed' = used + used'
              guard $ newUsed' <= capa
              let gScore' = gScore + 1
                  targetNode' = if coord' == targetNode then coord else targetNode
                  fScore' = gScore' + estimateDist targetNode' coord'
                  ns' = M.insert coord newUsed' $ M.insert coord' 0 ns
                  ss' = (coord', targetNode', ns')
              guard case gScores M.!? ss' of
                Nothing -> True
                Just g -> gScore' < g
              pure (ss', fScore', gScore')
            gScores' =
              foldr
                (\(ss', _, gScore') -> M.insert ss' gScore')
                gScores
                nexts
            q2 =
              foldr
                (\(ss', fScore', gScore') -> PQ.insert ss' (Arg fScore' gScore'))
                q1
                nexts
         in aStar capacities gScores' q2

instance Solution Day22 where
  solutionRun _ SolutionContext {getInputS, answerShow, terminal} = do
    nodes <- fmap (consumeOrDie nodeP) . drop 2 . lines <$> getInputS
    let viablePairs = do
          ((ca, a), xs0) <- pick nodes
          (cb, b) <- xs0
          guard $ nUsed a /= 0 && nUsed a <= nAvail b
          pure (ca, cb)
    answerShow $ length viablePairs
    do
      let Just (yMax, xMax) = checkCompleteness $ fmap fst nodes
          [(theEmpty, _)] = filter ((== 0) . nUsed . snd) nodes
          target = (0, xMax)
          shouldNormalizeInput =
            {-
              We want to run A* as it is on small examples to verify its correctness,
              but for larger inputs, we really need to normalize
              to get any reasonable run time.
             -}
            yMax * xMax > 25
          capacities :: M.Map Coord Int
          capacities = M.fromList $ (fmap . second) (f . nSize) nodes
            where
              f = if shouldNormalizeInput then tr else id
              -- See assumption #4 above.
              tr v
                | v >= 85 && v <= 94 = 80
                | v >= 500 = 500
                | otherwise = error "unexpected capacity"

          initNs = M.fromList $ (fmap . second) (f . nUsed) nodes
            where
              f = if shouldNormalizeInput then tr else id
              -- See assumption #4 above.
              tr v
                | v == 0 = 0
                | v >= 64 && v <= 73 = 70
                | v >= 490 = 490
                | otherwise = error "unexpected used"
          initSs :: SearchState
          initSs = (theEmpty, target, initNs)
      when (isJust terminal) do
        forM_ [0 .. yMax] \y -> do
          let render x
                | (y, x) == target = 'G'
                | u == 0 = 'E'
                | u >= 400 = '#'
                | otherwise = '.'
                where
                  u = initNs M.! (y, x)
          putStrLn $ fmap render [0 .. xMax]
        putStrLn "Used values before normalization:"
        print $ nubSort $ fmap (nUsed . snd) nodes
        putStrLn "Size values before normalization: "
        print $ nubSort $ fmap (nSize . snd) nodes
      answerShow $
        aStar
          capacities
          (M.singleton initSs 0)
          (PQ.singleton initSs $ Arg (estimateDist target theEmpty) 0)
