{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2016.Day2
  (
  )
where

import Control.Applicative
import Data.List
import qualified Data.Map.Strict as M
import Data.Monoid
import Javran.AdventOfCode.GridSystem.RowThenCol.Uldr
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day2 deriving (Generic)

dirP :: ReadP Dir
dirP =
  (U <$ char 'U')
    <++ (D <$ char 'D')
    <++ (L <$ char 'L')
    <++ (R <$ char 'R')

isInRange :: Coord -> Bool
isInRange = inRange ((0, 0), (2, 2))

isInRange2 :: Coord -> Bool
isInRange2 = (<= 2) . manhattan (0, 0)

applyDirBoundedBy :: (Coord -> Bool) -> Dir -> Coord -> Coord
applyDirBoundedBy isIn d c =
  if isIn c' then c' else c
  where
    c' = applyDir d c

applyDirs :: (Coord -> Bool) -> [Dir] -> Coord -> Coord
applyDirs isIn = appEndo . getDual . foldMap (Dual . Endo . applyDirBoundedBy isIn)

toCode :: Coord -> Int
toCode (r, c) = r * 3 + c + 1

toCode2 :: Coord -> Char
toCode2 = (m M.!)
  where
    coords = [coord | r <- [-2 .. 2], c <- [-2 .. 2], let coord = (r, c), isInRange2 coord]
    m = M.fromList $ zip coords $ ['1' .. '9'] <> ['A' .. 'D']

instance Solution Day2 where
  solutionRun _ SolutionContext {getInputS, answerShow, answerS} = do
    xs <- fmap (consumeOrDie (many dirP)) . lines <$> getInputS
    do
      let r = tail $ scanl' (\loc ds -> applyDirs isInRange ds loc) (1, 1) xs
      answerShow $ digitsToInt @Int $ fmap toCode r
    do
      let r = tail $ scanl' (\loc ds -> applyDirs isInRange2 ds loc) (0, -2) xs
      answerS $ fmap toCode2 r
