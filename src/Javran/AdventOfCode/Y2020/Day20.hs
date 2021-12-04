{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2020.Day20
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Bits
import Data.Bool
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (many)

data Day20

tileLen :: Int
tileLen = 10

halfLen :: Int
halfLen = tileLen `quot` 2

coords :: [Int]
coords = [0 .. tileLen -1]

pickInOrder :: [a] -> [] (a, [a])
pickInOrder [] = []
pickInOrder (x : xs) = (x, xs) : pickInOrder xs

{-
  (<lo>, <hi>)

  for (r,c):

  - when r < 5: `c + r * 10`, index into <lo>
  - when r >= 5: `c + (r-5)*10`, index into <hi>

 -}
newtype PackedTile = PackedTile (Word64, Word64)

toPackedTile :: [[Bool]] -> PackedTile
toPackedTile xs =
  checkShape
    `seq` PackedTile (packToWord loXs, packToWord hiXs)
  where
    checkShape =
      (length xs == tileLen
         || error "invalid # of rows")
        && (let (y : ys) = fmap length xs
             in (y == tileLen && all (== y) ys)
                  || error "invalid # of cols")
    (loXs, hiXs) = splitAt halfLen xs
    packToWord :: [[Bool]] -> Word64
    packToWord yys = appEndo setup 0
      where
        setup = mconcat $ do
          (r, ys) <- zip [0 :: Int ..] yys
          (c, y) <- zip [0 :: Int ..] ys
          let i = c + r * tileLen
          if y
            then pure $ Endo (`setBit` i)
            else mempty

type Tile = (Int, Int) -> Bool

toTile :: PackedTile -> Tile
toTile (PackedTile (lo, hi)) (r, c) =
  if r < halfLen
    then testBit lo (c + r * tileLen)
    else testBit hi (c + (r - halfLen) * tileLen)

type TracedTile = (Int, Tile)

parseTile :: [String] -> TracedTile
parseTile (t : xs) = (fromJust (consumeAllWithReadP tileNumP t), toTile . toPackedTile . (fmap . fmap) tr $ xs)
  where
    tr '#' = True
    tr '.' = False
    tr _ = error "invalid input"
    tileNumP = string "Tile " *> decimal1P <* char ':'
parseTile [] = error "invalid input"

flipVert :: Tile -> Tile
flipVert origTile (r, c) = origTile (tileLen -1 - r, c)

rotateCwQt :: Tile -> Tile
rotateCwQt origTile (r, c) = origTile (tileLen -1 - c, r)

renderTile :: Tile -> [String]
renderTile t =
  fmap (\r -> fmap (\c -> if t (r, c) then '#' else '.') [0 .. tileLen -1]) [0 .. tileLen -1]

pprTile :: Tile -> IO ()
pprTile t = do
  forM_ [0 .. tileLen -1] $ \r ->
    putStrLn (fmap (\c -> if t (r, c) then '#' else '.') [0 .. tileLen -1])

allTransforms :: Tile -> [(Int, Tile)]
allTransforms t0 = zip [0 ..] $ do
  t1 <- [t0, flipVert t0]
  take 4 (iterate rotateCwQt t1)

pprAllTransforms :: Tile -> IO ()
pprAllTransforms t = do
  let (l0, l1) = splitAt 4 (allTransforms t)
      ts0 = fmap (renderTile . snd) l0
      ts1 = fmap (renderTile . snd) l1
  mapM_ putStrLn $ fmap (intercalate "  ") $ transpose ts0
  putStrLn ""
  mapM_ putStrLn $ fmap (intercalate "  ") $ transpose ts1
  putStrLn ""

type Udlr = ((Int, Int), (Int, Int))

tileEdges :: Tile -> Udlr
tileEdges t =
  ( ( decodeBinary $ fmap (\c -> t (0, c)) coords
    , decodeBinary $ fmap (\c -> t (tileLen -1, c)) coords
    )
  , ( decodeBinary $ fmap (\r -> t (r, 0)) coords
    , decodeBinary $ fmap (\r -> t (r, tileLen -1)) coords
    )
  )

tileEdgeNums :: Tile -> [(Int, Int)]
tileEdgeNums t = do
  (i, t') <- allTransforms t
  let ((u, d), (l, r)) = tileEdges t'
  (i {- stands for a specific orientation for a tile -},) <$> [u, d, l, r]

instance Solution Day20 where
  solutionIndex _ = (2020, 20)
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    tracedTiles <- fmap parseTile . filter (not . null) . splitOn [""] . lines <$> getInputS
    let tracedEdgeNums = (fmap . second) tileEdgeNums tracedTiles
        edgeRepToTileIds = IM.fromListWith (<>) $ do
          (i, ens) <- tracedEdgeNums
          (_, en) <- ens
          pure (en, IS.singleton i)
        possibleConns = IM.fromListWith (<>) $ do
          (_e, tileIdSet) <- IM.toList edgeRepToTileIds
          (a, as) <- pickInOrder $ IS.toList tileIdSet
          b <- as
          [(a, IS.singleton b), (b, IS.singleton a)]
        edgeTiles =
          -- we can guess an edge tile if it only has 2 possible connections
          filter (\(v, es) -> IS.size es == 2) $ IM.toList possibleConns
        edgeTileIds = fmap fst edgeTiles
    -- it so happens that this is sufficient for both example and my input.
    -- enforce that we have exactly 4 elements
    [_, _, _, _] <- pure edgeTileIds
    answerShow $ product edgeTileIds
