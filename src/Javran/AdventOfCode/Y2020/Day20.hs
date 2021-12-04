{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Javran.AdventOfCode.Y2020.Day20
  (
  )
where

import Control.Monad
import Data.Bifunctor
import Data.Bits
import Data.Bool
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Semigroup
import qualified Data.Set as S
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
          (r, ys) <- zip [0 ..] yys
          (c, y) <- zip [0 ..] ys
          let i = c + r * tileLen
          if y
            then pure $ Endo (`setBit` i)
            else mempty

{-
  We keep the underlying structure intact and simply export a function
  that allows us to index into the tile.

  If we want to rotate or flip a tile, we pre-compose a coordinate transformation.
  This way we avoids actually performing any transformation on the underlying data entirely.

  The trade off is that this precomposition could be multiple layers that we have to compute
  on each access. But given that we pre-compose at most 4 times
  (worse case is one vertical flip followed by 3 rotations),
  this is a price we are willing to pay.
 -}
type Tile = (Int, Int) -> Bool

toTile :: PackedTile -> Tile
toTile (PackedTile (lo, hi)) (r, c) =
  if r < halfLen
    then testBit lo (c + r * tileLen)
    else testBit hi (c + (r - halfLen) * tileLen)

type TracedTile = (Int, Tile)

parseTile :: [String] -> TracedTile
parseTile (t : xs) =
  ( fromJust (consumeAllWithReadP tileNumP t)
  , toTile . toPackedTile . (fmap . fmap) tr $ xs
  )
  where
    tr '#' = True
    tr '.' = False
    tr _ = errInvalid
    tileNumP = string "Tile " *> decimal1P <* char ':'
parseTile [] = errInvalid

data CoordTransformers a = CoordTransformers
  { flipVert :: forall b. (a -> b) -> (a -> b)
  , rotateCwQt :: forall b. (a -> b) -> (a -> b)
  }

{-
  Side note:

  notice that both flipVert and rotateCwQt are function precompositions,
  meaning the following will work:

  flipVert = getOp . contramap (\(r, c) -> (sideLen -1 - r, c)) . Op

  looks nicer if we ignore those newtype wrappers,
  but admittedly less readable.

 -}

{- HLINT ignore mkCoordTransformers "Use first" -}
mkCoordTransformers :: Int -> CoordTransformers (Int, Int)
mkCoordTransformers sideLen =
  CoordTransformers
    { flipVert = \f (r, c) -> f (sideLen -1 - r, c)
    , rotateCwQt = \f (r, c) -> f (sideLen -1 - c, r)
    }

type TileAlts = [(Int, Tile)]

allTransforms :: Tile -> TileAlts
allTransforms t0 = zip [0 ..] $ do
  let CoordTransformers {flipVert, rotateCwQt} = mkCoordTransformers tileLen
  t1 <- [t0, flipVert t0]
  take 4 (iterate rotateCwQt t1)

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
        (_i, curTile) <- ts
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

      for a tile to be put in that place:
      - its top edge must match down edge of (r-1,c)
      - its left edge must match right edge of (r,c-1)

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
          {-
            this row is done if we ends up having right edige of (r,c)
            that cannot be connected to any other tiles.
           -}
          if IS.member cRight deadEdgeReps
            then (r + 1, 0)
            else (r, c + 1)
    solveTiles deadEdgeReps nextCoord remainingTiles' solution'

{-
  verifies a solution and compute tile dimensions.

  - a complete solution should form a rectangle
  - to make things easier, it seems to be the case that we always end up
    with a square. this function also verifies this assumption.

 -}
verifyDims :: S.Set TileCoord -> Maybe (Int, Int)
verifyDims ts = do
  let Just (Max maxR, Max maxC) = foldMap (\(r, c) -> Just (Max r, Max c)) ts
  guard $ all (`S.member` ts) [(r, c) | r <- [0 .. maxR], c <- [0 .. maxC]]
  guard $ maxR == maxC
  pure (maxR + 1, maxC + 1)

type Sea = V.Vector (V.Vector Bool)

constructSea :: (Int, Int) -> M.Map TileCoord Tile -> Sea
constructSea (tileRows, tileCols) tiles = V.fromList (fmap V.fromList flattened)
  where
    flattened :: [[Bool]]
    flattened =
      concatMap
        (\tileRow ->
           fmap concat $
             transpose $
               fmap
                 (\tileCol -> unpackTile $ tiles M.! (tileRow, tileCol))
                 [0 .. tileCols -1])
        [0 .. tileRows -1]
    unpackTile :: Tile -> [[Bool]]
    unpackTile t = do
      fmap (\r -> fmap (\c -> t (r, c)) [1 .. tileLen -2]) [1 .. tileLen -2]

type SeaViewer = (Int, Int) -> Bool

-- Bidi short for bi-directional.
type SeaBidi =
  ( SeaViewer
  , (Int, Int) -> (Int, Int) -- translates back to underlying coord.
  )

mkSeaBidis :: Sea -> [SeaBidi]
mkSeaBidis sea = do
  let directViewer (r, c) = sea V.! r V.! c
      directBidi = (directViewer, id)
      seaLen = V.length sea

      CoordTransformers {flipVert, rotateCwQt} = mkCoordTransformers seaLen

      sFlipVert :: SeaBidi -> SeaBidi
      sFlipVert = bimap flipVert flipVert

      sRotateCwQt :: SeaBidi -> SeaBidi
      sRotateCwQt = bimap rotateCwQt rotateCwQt
  v <- [directBidi, sFlipVert directBidi]
  take 4 (iterate sRotateCwQt v)

seaMonsterDims :: (Int, Int)
seaMonsterParts :: S.Set (Int, Int)
(seaMonsterDims, seaMonsterParts) = (dims, parts)
  where
    dims = (length art, length (head art))
    art =
      [ "                  # "
      , "#    ##    ##    ###"
      , " #  #  #  #  #  #   "
      ]
    parts = S.fromList $ do
      (r, row) <- zip [0 ..] art
      (c, x) <- zip [0 ..] row
      guard $ x == '#'
      pure (r, c)

findSeaMonsters :: Int -> SeaBidi -> [S.Set (Int, Int)]
findSeaMonsters seaLen (viewer, backTranslate) = do
  let (smRows, smCols) = seaMonsterDims
  r0 <- takeWhile (\r' -> r' + smRows -1 <= seaLen -1) [0 ..]
  c0 <- takeWhile (\c' -> c' + smCols -1 <= seaLen -1) [0 ..]
  let translatedSm = S.map (bimap (+ r0) (+ c0)) seaMonsterParts
  guard $ all viewer translatedSm
  pure (S.map backTranslate translatedSm)

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
          filter (\(_v, es) -> IS.size es == 2) $ IM.toList possibleConns
        edgeTileIds = fmap fst edgeTiles
    -- it so happens that this is sufficient for both example and my input.
    -- enforce that we have exactly 4 elements
    [ topLeftTileId {- pick a random one as top-left corner -}
      , _
      , _
      , _
      ] <-
      pure edgeTileIds
    -- just knowing what those 4 tiles are allows us to answer first question.
    answerShow $ product edgeTileIds
    let (orientedTopLeftTile, remainingTiles, deadEdgeReps) =
          prepareTileSolving tracedTiles topLeftTileId edgeRepToTileIds
        solvedTiles =
          head $
            solveTiles
              deadEdgeReps
              (0, 1)
              remainingTiles
              (M.singleton (0, 0) orientedTopLeftTile)
    Just tileDims <- pure (verifyDims (M.keysSet solvedTiles))
    let sea = constructSea tileDims solvedTiles
        seaLen = V.length sea
        monsters = do
          bd <- mkSeaBidis sea
          findSeaMonsters seaLen bd
        allMonsterParts :: S.Set (Int, Int)
        allMonsterParts = S.unions monsters
    let visualize = False
    when visualize $
      forM_ (zip [0 :: Int ..] (V.toList sea)) $ \(r, rs) -> do
        putStrLn
          (fmap
             (\(c, x) ->
                if S.member (r, c) allMonsterParts
                  then 'O'
                  else bool '.' '#' x)
             $ zip [0 ..] (V.toList rs))
    answerShow $ sum do
      r <- [0 .. seaLen -1]
      c <- [0 .. seaLen -1]
      guard $ S.notMember (r, c) allMonsterParts
      guard $ sea V.! r V.! c
      pure (1 :: Int)
