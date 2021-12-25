{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Javran.AdventOfCode.Y2021.Day25
  (
  )
where

import qualified Data.Set as S
import Javran.AdventOfCode.Prelude

data Day25 deriving (Generic)

type Dims = (Int, Int)

type Coord = (Int, Int) -- row then col.

type World =
  ( S.Set Coord -- east facing
  , S.Set Coord -- south facing
  )

eastNext :: Dims -> Coord -> Coord
eastNext (_rows, cols) (r, c) = (r, (c + 1) `rem` cols)

southNext :: Dims -> Coord -> Coord
southNext (rows, _cols) (r, c) = ((r + 1) `rem` rows, c)

stepEast :: Dims -> World -> World
stepEast dims (es, ss) = (es', ss)
  where
    occupied = S.union es ss
    es' = S.fromList do
      coord <- S.toList es
      let coord' = eastNext dims coord
      if S.member coord' occupied
        then pure coord
        else pure coord'

stepSouth :: Dims -> World -> World
stepSouth dims (es, ss) = (es, ss')
  where
    occupied = S.union es ss
    ss' = S.fromList do
      coord <- S.toList ss
      let coord' = southNext dims coord
      if S.member coord' occupied
        then pure coord
        else pure coord'

step :: Dims -> World -> World
step dims = stepSouth dims . stepEast dims

instance Solution Day25 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- lines <$> getInputS
    let rows = length xs
        cols = length (head xs)
        dims = (rows, cols)
        world = mconcat do
          (r, rs) <- zip [0 ..] xs
          (c, x) <- zip [0 ..] rs
          let coord = (r, c)
          case x of
            '.' -> pure mempty
            '>' -> pure (S.singleton coord, mempty)
            'v' -> pure (mempty, S.singleton coord)
            _ -> errInvalid
        progression = iterate (step dims) world
        (i, _) : _ =
          dropWhile
            (\(_, (a, b)) -> a /= b)
            $ zip [1 :: Int ..] (zip progression (tail progression))
    answerShow i
