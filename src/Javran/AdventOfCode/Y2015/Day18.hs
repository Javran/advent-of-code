{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2015.Day18
  (
  )
where

import Control.Monad
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Javran.AdventOfCode.GridSystem.RowThenCol.Uldr
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.TestExtra

data Day18 deriving (Generic)

{-
  TODO: the performance could have been better since we can
  just keep the world a sorted list of Coords.
  (actually, we only need Map for computing the adjacent-contribution)
  But I don't think it's necessary to go that extra mile (for now).
 -}
type CoordSet = S.Set Coord

_pprWorld :: Int -> CoordSet -> IO ()
_pprWorld size w =
  forM_ [0 .. size -1] \r -> do
    let render c = if S.member (r, c) w then '#' else '.'
    putStrLn $ fmap render [0 .. size -1]

adjacents :: Coord -> [Coord]
adjacents coord@(r, c) = do
  r' <- [r -1 .. r + 1]
  c' <- [c -1 .. c + 1]
  let coord' = (r', c')
  coord' <$ guard (coord /= coord')

step :: Int -> CoordSet -> CoordSet
step size w =
  S.fromDistinctAscList $ mapMaybe mightKeep $ M.toList contribs
  where
    mightKeep (coord, cnt) =
      coord
        <$ guard
          if S.member coord w
            then cnt `elem` [2, 3]
            else cnt == 3

    contribs = M.fromListWith (+) do
      cur <- S.toList w
      adj <- adjacents cur
      guard $ inRange ((0, 0), (size -1, size -1)) adj
      pure (adj, 1 :: Int)

instance Solution Day18 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (ex, rawInput) <- consumeExtra getInputS
    let raw = lines rawInput
        size = length raw
        (stepsP1, stepsP2) = singleLineExtra (100, 100) ex
        world = S.fromDistinctAscList do
          (r, rs) <- zip [0 ..] raw
          (c, '#') <- zip [0 ..] rs
          pure (r, c)
    do
      let progression = iterate (step size) world
      answerShow $ S.size (progression !! stepsP1)
    do
      let corners = S.fromAscList [(0, 0), (0, n'), (n', 0), (n', n')]
            where
              n' = size - 1
          addCorners = S.union corners
          step2 w = addCorners $ step size (addCorners w)
          progression = iterate step2 world
      answerShow $ S.size (progression !! stepsP2)
