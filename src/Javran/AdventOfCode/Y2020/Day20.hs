{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
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
import Data.Semigroup
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

type TileAlts = [(Int, Tile)]

type TracedTileAlts = (Int {- tile id -}, TileAlts)

allTransforms :: Tile -> TileAlts
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

prepareTileSolving
  :: [TracedTile]
  -> Int
  -> IM.IntMap IS.IntSet
  -> (Tile, IM.IntMap TileAlts, IS.IntSet)
prepareTileSolving
  tracedTiles
  topLeftTileId
  edgeRepToTileIds =
    ( topLeftTile
    , IM.union tmpL tmpR
    , deadEdgeReps
    )
    where
      initTracedTileAltsMap :: IM.IntMap TileAlts
      initTracedTileAltsMap = IM.fromList $ do
        (tId, tile) <- tracedTiles
        pure (tId, allTransforms tile)
      (tmpL, Just ts, tmpR) = IM.splitLookup topLeftTileId initTracedTileAltsMap
      topLeftTile = head $ do
        t@(i, curTile) <- ts
        let ((u, _d), (l, _r)) = tileEdges curTile
        guard $ IS.member u deadEdgeReps && IS.member l deadEdgeReps
        pure curTile
      deadEdgeReps = IS.fromList $ do
        -- compute edge reps that does not connect anything
        (edgeRep, xs) <- IM.toList edgeRepToTileIds
        guard $ IS.size xs == 1
        pure edgeRep

type TileCoord = (Int, Int)

type TileSolution = M.Map TileCoord Tile

solveTiles :: IS.IntSet -> (Int, Int) -> IM.IntMap TileAlts -> TileSolution -> [TileSolution]
solveTiles deadEdgeReps (r, c) remainingTiles solution
  | IM.null remainingTiles = [solution]
  | otherwise = do
    {-
      Find an appropriate tile to put on row r, col c.
      We want to solve in row-major order,
      meaning we can assume (r-1,c) and (r,c-1) are already solved, if present.
     -}
    let mUpEdge, mLeftEdge :: Maybe Int
        mUpEdge = do
          guard $ r /= 0
          let topTile = solution M.! (r -1, c)
              ((_up, down), _) = tileEdges topTile
          pure down
        mLeftEdge = do
          guard $ c /= 0
          let leftTile = solution M.! (r, c -1)
              (_, (_left, right)) = tileEdges leftTile
          pure right
    (tileId, alts) <- IM.toList remainingTiles
    (_, candidateTile) <- alts
    let ((cUp, _cDown), (cLeft, cRight)) = tileEdges candidateTile
    guard $ maybe True (== cUp) mUpEdge
    guard $ maybe True (== cLeft) mLeftEdge
    let solution' = M.insert (r, c) candidateTile solution
        remainingTiles' = IM.delete tileId remainingTiles
        nextCoord =
          if IS.member cRight deadEdgeReps
            then (r + 1, 0)
            else (r, c + 1)
    solveTiles deadEdgeReps nextCoord remainingTiles' solution'

verifyDims :: S.Set TileCoord -> Maybe (Int, Int)
verifyDims ts = do
  let Just (Max maxR, Max maxC) = foldMap (\(r, c) -> Just (Max r, Max c)) ts
  guard $ all (`S.member` ts) [(r, c) | r <- [0 .. maxR], c <- [0 .. maxC]]
  pure (maxR + 1, maxC + 1)

type Sea = V.Vector (V.Vector Bool)

constructSea :: (Int, Int) -> M.Map TileCoord Tile -> Sea
constructSea (tileRows, tileCols) tiles = V.fromList (fmap V.fromList flattened)
  where
    flattened :: [[Bool]]
    flattened =
      concat $
        fmap
          (\tileRow ->
             fmap concat $
               transpose $
                 fmap
                   (\tileCol -> unpackTile $ tiles M.! (tileRow, tileCol))
                   [0 .. tileCols -1])
          [0 .. tileRows -1]
    unpackTile :: Tile -> [[Bool]]
    unpackTile t =
      fmap (\r -> fmap (\c -> t (r, c)) [1 .. tileLen -2]) [1 .. tileLen -2]

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
    [ topLeftTileId {- pick a random one as top-left corner -}
      , _
      , _
      , _
      ] <-
      pure edgeTileIds
    answerShow $ product edgeTileIds
    let (orientedTopLeftTile, remainingTiles, deadEdgeReps) =
          prepareTileSolving tracedTiles topLeftTileId edgeRepToTileIds
        solvedTiles = head $ solveTiles deadEdgeReps (0, 1) remainingTiles (M.singleton (0, 0) orientedTopLeftTile)
    Just tileDims@(rows, cols) <- pure (verifyDims (M.keysSet solvedTiles))
    let sea = constructSea tileDims solvedTiles
    forM_ sea $ \rs -> do
      putStrLn (fmap (bool '.' '#') $ V.toList rs)
