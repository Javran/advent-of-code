{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Javran.AdventOfCode.Y2019.Day24
  (
  )
where

import Control.Monad
import Data.Bits
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Javran.AdventOfCode.Prelude

data Day24 deriving (Generic)

type Coord = (Int, Int)

type World = S.Set Coord

isIn5x5 :: Coord -> Bool
isIn5x5 = inRange ((0, 0), (4, 4))

mkStep :: Ord coord => (coord -> [coord]) -> S.Set coord -> S.Set coord
mkStep getAdjacents w =
  S.union
    (S.filter
       (\coord -> case adjs M.!? coord of
          Just 1 -> True
          _ -> False)
       w)
    newBorns
  where
    newBorns =
      M.keysSet
        (M.filter (\v -> v == 1 || v == 2) $ M.withoutKeys adjs w)
    adjs = M.fromListWith (+) do
      coord <- S.toList w
      coord' <- getAdjacents coord
      pure (coord', 1 :: Int)

step :: World -> World
step = mkStep $ filter isIn5x5 . udlrOfCoord

encodeWorld :: World -> Int
encodeWorld = S.foldl' (\acc (r, c) -> setBit acc (r * 5 + c)) 0

type Coord2 = (Int, Coord)

type World2 = S.Set Coord2

adjacents2 :: Coord2 -> [Coord2]
adjacents2 (level, coord) = case coord of
  (2, 2) -> error "invalid coord"
  (r, c) ->
    (do
       coord' <- udlrOfCoord coord
       guard $ coord' /= (2, 2) && isIn5x5 coord'
       pure (level, coord'))
      <> [(level -1, (1, 2)) | r == 0]
      <> [(level -1, (3, 2)) | r == 4]
      <> [(level -1, (2, 1)) | c == 0]
      <> [(level -1, (2, 3)) | c == 4]
      <> [(level + 1, (0, c')) | coord == (1, 2), c' <- [0 .. 4]]
      <> [(level + 1, (4, c')) | coord == (3, 2), c' <- [0 .. 4]]
      <> [(level + 1, (r', 0)) | coord == (2, 1), r' <- [0 .. 4]]
      <> [(level + 1, (r', 4)) | coord == (2, 3), r' <- [0 .. 4]]

step2 :: World2 -> World2
step2 = mkStep adjacents2

instance Solution Day24 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (ex, rawInput) <- consumeExtraLeadingLines <$> getInputS
    let initWorld :: World
        initWorld = S.fromList do
          (r, rs) <- zip [0 ..] (lines rawInput)
          (c, '#') <- zip [0 ..] rs
          pure (r, c)
        ans =
          unfoldr
            (\(curW, discovered, found) -> do
               guard $ not found
               let w' = step curW
                   found' = S.member w' discovered
               pure (w', (w', S.insert curW discovered, found')))
            (initWorld, S.empty, False)
    answerShow (encodeWorld $ last ans)
    let initWorld2 = S.map (0,) $ S.delete (2, 2) initWorld
        progression = iterate step2 initWorld2
    case ex of
      Nothing ->
        answerShow (S.size $ progression !! 200)
      Just _ ->
        answerShow (S.size $ progression !! 10)
