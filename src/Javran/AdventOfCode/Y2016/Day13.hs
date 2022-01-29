{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2016.Day13
  (
  )
where

import Control.Monad
import Data.Bits
import Data.Function
import qualified Data.Map.Strict as M
import qualified Data.PSQueue as PQ
import Data.Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.TestExtra

data Day13 deriving (Generic)

type Coord = (Int, Int) -- X then Y

data Cell = Open | Wall deriving (Ord, Eq)

mkMapInfo :: Int -> Coord -> Cell
mkMapInfo seed (x, y) = if even (popCount v) then Open else Wall
  where
    -- same polynomial, re-arranged to slightly reduce the amount of operations.
    v = x * (x + 3 + 2 * y) + y + y * y + seed

adjacents :: (Coord -> Cell) -> Coord -> [Coord]
adjacents mi coord = do
  coord'@(x', y') <- udlrOfCoord coord
  guard $ x' >= 0 && y' >= 0 && mi coord' == Open
  pure coord'

aStar
  :: (Coord -> Cell)
  -> Coord
  -> PQ.PSQ Coord (Arg Int Int)
  -> M.Map Coord Int
  -> Int
aStar mi goal = fix \search q0 dists -> case PQ.minView q0 of
  Nothing -> error "queue exhausted"
  Just (u PQ.:-> (Arg _fScore distU), q1) ->
    if u == goal
      then distU
      else
        let nexts = do
              v <- adjacents mi u
              let mDistV = dists M.!? v
                  distV' = distU + 1
                  fScore' = distV' + manhattan v goal
              guard $ maybe True (distV' <) mDistV
              pure (v, distV', Arg fScore' distV')
            q2 = foldr upd q1 nexts
              where
                upd (v, _, prio') = PQ.insert v prio'
            dists' = foldr upd dists nexts
              where
                upd (v, distV', _) = M.insert v distV'
         in search q2 dists'

bfs :: (Coord -> Cell) -> S.Set Coord -> (Seq.Seq (Coord, Int) -> Int)
bfs mi discovered = \case
  Seq.Empty -> S.size discovered
  (u, dist) Seq.:<| q1 ->
    let nexts = do
          guard $ dist < 50
          v <- adjacents mi u
          guard $ S.notMember v discovered
          pure (v, dist + 1)
        discovered' = foldr (\(v, _) -> S.insert v) discovered nexts
        q2 = q1 <> Seq.fromList nexts
     in bfs mi discovered' q2

instance Solution Day13 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (extraOps, rawInput) <- consumeExtra getInputS
    let seed = read @Int . head . lines $ rawInput
        mapInfo = mkMapInfo seed
        start = (1, 1)
        target = singleLineExtra (31, 39) extraOps
    answerShow $
      aStar
        mapInfo
        target
        (PQ.singleton start (Arg (manhattan start target) 0))
        (M.singleton start 0)
    answerShow $
      bfs mapInfo (S.singleton start) (Seq.singleton (start, 0))
