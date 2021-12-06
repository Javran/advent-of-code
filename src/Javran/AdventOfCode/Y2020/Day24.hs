{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2020.Day24
  (
  )
where

import Control.Applicative
import Control.Monad
import Data.Bits
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, many)

data Day24 deriving (Generic)

data Dir = E | SE | SW | W | NW | NE deriving (Show, Bounded, Enum)

dirP :: ReadP Dir
dirP =
  (E <$ char 'e')
    <++ (W <$ char 'w')
    <++ (char 'n' *> sub NE NW)
    <++ (char 's' *> sub SE SW)
  where
    sub dE dW = (dE <$ char 'e') <++ (dW <$ char 'w')

-- Ref: https://www.redblobgames.com/grids/hexagons/#coordinates-axial
newtype Axial = Axial (Int, Int) deriving (Show, Ord, Eq)

dirToVec :: Dir -> (Int, Int)
dirToVec = \case
  NW -> (0, -1)
  NE -> (1, -1)
  W -> (-1, 0)
  E -> (1, 0)
  SW -> (-1, 1)
  SE -> (0, 1)

applyDir :: Dir -> Axial -> Axial
applyDir d (Axial (q, r)) = Axial (q + dq, r + dr)
  where
    (dq, dr) = dirToVec d

relDirsToAxial :: [Dir] -> Axial
relDirsToAxial ds =
  appEndo (getDual $ foldMap (\d -> Dual (Endo (applyDir d))) ds) orig
  where
    orig = Axial (0, 0)

type TileConfig = M.Map Axial Bool

step :: TileConfig -> TileConfig
step m = M.fromList do
  (ax, adjCnt) <- M.toList adjBlackCounts
  let handleWhite = do
        guard $ adjCnt == 2
        pure (ax, True)
  case m M.!? ax of
    Nothing -> handleWhite
    Just False -> handleWhite
    Just True -> do
      guard $ not (adjCnt == 0 || adjCnt > 2)
      pure (ax, True)
  where
    {-
      notice that the key of this Map are all axial coords
      that we need to consider -
      other tiles simply have no black tiles adjacent to them.
     -}
    adjBlackCounts = M.fromListWith (+) do
      (ax, True) <- M.toList m
      d <- universe @Dir
      let ax' = applyDir d ax
      pure (ax', 1 :: Int)

instance Solution Day24 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (fromJust . consumeAllWithReadP (many dirP)) . lines <$> getInputS
    let tiles = M.fromListWith xor $ do
          ax <- fmap relDirsToAxial xs
          pure (ax, True)
    answerShow $ countLength id (M.elems tiles)
    answerShow $ countLength id (iterate step tiles !! 100)
