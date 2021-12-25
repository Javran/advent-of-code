{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2021.Day25
  (
  )
where

import Control.Lens
import Data.Bits
import Data.Coerce
import qualified Data.IntSet as IS
import Javran.AdventOfCode.Prelude

data Day25 deriving (Generic)

type Dims = (Int, Int)

newtype Coord = Coord Int {- 0~7 row, 8~15 col -}
  deriving (Ord, Eq)

type CoordSet = IS.IntSet

type World =
  ( CoordSet -- east facing
  , CoordSet -- south facing
  )

toCoord :: (Int, Int) -> Coord
toCoord (r, c) = Coord (lo .|. shift hi 8)
  where
    lo = r .&. 0xFF
    hi = c .&. 0xFF

fromCoord :: Coord -> (Int, Int)
fromCoord (Coord v) = (r, c)
  where
    r = v .&. 0xFF
    c = shift v (-8)

eastNext, southNext :: Dims -> Coord -> Coord
(eastNext, southNext) = (withCoord eastNext', withCoord southNext')
  where
    withCoord f dim = toCoord . f dim . fromCoord
    eastNext' (_rows, cols) (r, c) = (r, (c + 1) `rem` cols)
    southNext' (rows, _cols) (r, c) = ((r + 1) `rem` rows, c)

stepBy :: (Coord -> Coord) -> (Coord -> Bool) -> CoordSet -> CoordSet
stepBy nextCoord isConflict xs = IS.fromList do
  coord <- coerce @Int @Coord <$> IS.toList xs
  let coord' = nextCoord coord
  pure $
    coerce $
      if isConflict coord'
        then coord
        else coord'

isOccupied :: World -> Coord -> Bool
isOccupied (es, ss) =
  coerce $ (||) <$> (`IS.member` es) <*> (`IS.member` ss)

step :: Dims -> World -> World
step dims = stepSouth . stepEast
  where
    stepSouth w = w & _2 %~ stepBy (southNext dims) (isOccupied w)
    stepEast w = w & _1 %~ stepBy (eastNext dims) (isOccupied w)

instance Solution Day25 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- lines <$> getInputS
    let rows = length xs
        cols = length (head xs)
        dims = (rows, cols)
        initWorld = mconcat do
          (r, rs) <- zip [0 ..] xs
          (c, x) <- zip [0 ..] rs
          let coord = toCoord (r, c)
          pure case x of
            '.' -> mempty
            '>' -> (IS.singleton (coerce coord), mempty)
            'v' -> (mempty, IS.singleton (coerce coord))
            _ -> errInvalid
        progression = iterate (step dims) initWorld
        (i, _) : _ =
          dropWhile
            (\(_, (a, b)) -> a /= b)
            $ zip [1 :: Int ..] (zip progression (tail progression))
    if cols > 0xFF || rows > 0xFF
      then error "dims out of bound"
      else answerShow i
