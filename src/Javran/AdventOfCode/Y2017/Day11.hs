{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Javran.AdventOfCode.Y2017.Day11
  (
  )
where

import Control.Monad
import Control.Monad.Writer.CPS
import Data.Functor
import Data.Semigroup
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day11 deriving (Generic)

{-
  Again, we are using Axial coordinates:

  https://www.redblobgames.com/grids/hexagons/#coordinates-axial

  But this time (another use of this coordinate system is in Y2020.Day24),
  we need the "flat" variation.
 -}

data Dir = N | NW | SW | S | SE | NE deriving (Show)

dirP :: ReadP Dir
dirP = (char 'n' *> sub NW N NE) <++ (char 's' *> sub SW S SE)
  where
    sub w x e =
      (w <$ char 'w')
        <++ (e <$ char 'e')
        <++ pure x

newtype Axial = Axial (Int, Int) deriving (Show, Ord, Eq)

dirToVec :: Dir -> (Int, Int)
dirToVec = \case
  N -> (0, -1)
  NW -> (-1, 0)
  NE -> (1, -1)
  S -> (0, 1)
  SW -> (-1, 1)
  SE -> (1, 0)

applyDir :: Dir -> Axial -> Axial
applyDir d (Axial (q, r)) = Axial (q + dq, r + dr)
  where
    (dq, dr) = dirToVec d

axialDistToOrigin :: Axial -> Int
axialDistToOrigin (Axial (q, r)) = halve $ sum (fmap abs [q, q + r, r])

type TrackMaxDist = Writer (Maybe (Max Int))

applyDirM :: Dir -> Axial -> TrackMaxDist Axial
applyDirM d a = a' <$ tell (Just (Max (axialDistToOrigin a')))
  where
    a' = applyDir d a

instance Solution Day11 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    dirs <- consumeOrDie (dirP `sepBy` char ',') . head . lines <$> getInputS
    let (ans1, Just (Max ans2)) =
          runWriter
            (foldM (\cur d -> applyDirM d cur) (Axial (0, 0)) dirs
               <&> axialDistToOrigin)
    answerShow ans1
    answerShow ans2
