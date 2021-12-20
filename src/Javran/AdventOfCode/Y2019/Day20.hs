{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Javran.AdventOfCode.Y2019.Day20
  (
  )
where

import Control.Monad
import Control.Monad.State.Strict
import qualified Data.Array as Arr
import Data.Bifunctor
import Data.Char
import Data.Either
import Data.Foldable
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.PSQueue as PQ
import qualified Data.Set as S
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude

data Day20 deriving (Generic)

type Coord = (Int, Int)

data Dir = U | D | L | R deriving (Eq, Show)

applyDir :: Dir -> Coord -> Coord
applyDir = \case
  U -> first pred
  D -> first succ
  L -> second pred
  R -> second succ

data ParsedMap = ParsedMap
  { pmGraph :: M.Map Coord [Either String Coord]
  , pmPortals :: M.Map String [(Coord, Dir)]
  , pmStartEnd :: (Coord, Coord)
  , pmInnerOuter :: (MinMax2D Int Int, MinMax2D Int Int)
  }

parseMap :: Arr.Array Coord Char -> ParsedMap
parseMap rawFloor =
  ParsedMap
    { pmGraph
    , pmStartEnd = (startCoord, endCoord)
    , pmPortals
    , pmInnerOuter = (inner, outer)
    }
  where
    getCell coord = rawFloor Arr.! coord
    ([(startCoord, _)], [(endCoord, _)]) =
      ( pmPortals M.! "AA"
      , pmPortals M.! "ZZ"
      )
    pmGraph = M.unionsWith (<>) gPre
    pmPortals = M.unionsWith (<>) pPre
    Just outer = foldMap (Just . minMax2D) do
      (coord, x) <- Arr.assocs rawFloor
      guard $ x `elem` "#."
      pure coord

    Just inner = foldMap (Just . minMax2D) do
      let isInside =
            let MinMax2D ((minR, maxR), (minC, maxC)) = outer
             in inRange ((minR, minC), (maxR, maxC))
      (coord, ' ') <- Arr.assocs rawFloor
      guard $ isInside coord
      pure coord

    (gPre, pPre) = unzip do
      (coord@(r, c), '.') <- Arr.assocs rawFloor
      let tryDir dir coord0 p0 p1 = do
            let v = getCell coord0
            case v of
              '#' -> []
              '.' -> pure (Right coord0, Nothing)
              _
                | isAsciiUpper v -> do
                  let pTag = [getCell p0, getCell p1]
                  pure (Left pTag, Just (pTag, dir))
              _ -> errInvalid
      (val, mPortal) <-
        asum
          [ tryDir U (r -1, c) (r -2, c) (r -1, c)
          , tryDir D (r + 1, c) (r + 1, c) (r + 2, c)
          , tryDir L (r, c -1) (r, c -2) (r, c -1)
          , tryDir R (r, c + 1) (r, c + 1) (r, c + 2)
          ]
      pure
        ( M.singleton coord [val] -- one piece of miGraph
        , -- portal tag to this coord.
          maybe
            M.empty
            (\(tag, dir) -> M.singleton tag [(coord, dir)])
            mPortal
        )

data PortalSide = PsInner | PsOuter deriving (Show)

type Edges = M.Map Coord (Maybe (String, Dir, PortalSide))

data MapInfo = MapInfo
  { miGraph :: M.Map Coord Edges
  , miStartEnd :: (Coord, Coord)
  , miDist :: M.Map (MinMax Coord) Int
  , miInnerOuter :: (MinMax2D Int Int, MinMax2D Int Int)
  }

mkMapInfo :: ParsedMap -> MapInfo
mkMapInfo
  ParsedMap
    { pmGraph
    , pmStartEnd = miStartEnd
    , pmPortals
    , pmInnerOuter = miInnerOuter
    } =
    MapInfo
      { miStartEnd
      , miInnerOuter
      , miGraph
      , miDist = M.fromList do
          (u, vs) <- M.toList miGraph
          v <- M.keys vs
          guard $ u <= v
          pure (minMaxFromPair (u, v), 1)
      }
    where
      isOnInnerRim :: Coord -> Bool
      isOnInnerRim = inRange ((minR -1, minC -1), (maxR + 1, maxC + 1))
        where
          (MinMax2D ((minR, maxR), (minC, maxC)), _Outer) = miInnerOuter
      miGraph :: M.Map Coord Edges
      miGraph = M.mapWithKey connectPortal miGraphPre
        where
          connectPortal :: Coord -> [Either String Coord] -> Edges
          connectPortal coord vs = M.fromList (ls <> fmap (,Nothing) rs)
            where
              (lsPre, rs) = partitionEithers vs
              ls :: [(Coord, Maybe (String, Dir, PortalSide))]
              ls =
                mapMaybe
                  (\tag -> do
                     guard $ tag `notElem` ["AA", "ZZ"]
                     let [(c', _)] = filter ((/= coord) . fst) (pmPortals M.! tag)
                         Just dir = lookup coord (pmPortals M.! tag)
                     pure (c', Just (tag, dir, if isOnInnerRim coord then PsInner else PsOuter)))
                  lsPre

      miGraphPre :: M.Map Coord [Either String Coord]
      miGraphPre = pmGraph

debugMapInfo :: MapInfo -> IO ()
debugMapInfo
  MapInfo
    { miGraph
    , miStartEnd = (startCoord, endCoord)
    , miDist
    , miInnerOuter = (inner, outer)
    } = do
    let isOuter = inRange ((minR, minC), (maxR, maxC))
        MinMax2D ((minR, maxR), (minC, maxC)) = outer
        isInner = inRange ((a, c), (b, d))
          where
            MinMax2D ((a, b), (c, d)) = inner
        portalExtras :: M.Map Coord PortalSide
        portalExtras = M.fromList do
          (u, es) <- M.toList miGraph
          (_, Just (_, d, ps)) <- M.toList es
          pure (applyDir d u, ps)

    forM_ [minR - 2 .. maxR + 2] $ \r -> do
      let render c =
            if
                | Just p <- portalExtras M.!? coord ->
                  case p of
                    PsInner -> 'i'
                    PsOuter -> 'o'
                | isInner coord -> ' '
                | coord == startCoord -> 'S'
                | coord == endCoord -> 'E'
                | isOuter coord ->
                  case miGraph M.!? coord of
                    Nothing -> '█'
                    Just _ -> '.'
                | otherwise -> ' '
            where
              coord = (r, c)
      putStrLn (fmap render [minC - 4 .. maxC + 4])
    let showRoutes = True
    when showRoutes do
      putStrLn $ "start, end: " <> show (startCoord, endCoord)
      c <- forM (M.toAscList miDist) $ \(MinMax (c0, c1), dist) -> do
        case (miGraph M.!? c0, miGraph M.!? c1) of
          (Just edges0, Just edges1) -> do
            let e01 = edges0 M.! c1
                e10 = edges1 M.! c0
                renderExtra = \case
                  Nothing -> ""
                  Just (xs, _d, ps) ->
                    concat
                      [ " "
                      , xs
                      , ","
                      , case ps of
                          PsInner -> "I"
                          PsOuter -> "O"
                      ]
            putStrLn $
              show c0
                <> renderExtra e01
                <> " <=> "
                <> show c1
                <> renderExtra e10
                <> ": "
                <> show dist
            pure (1 :: Int)
          _ -> pure 0
      print $ sum c

runSpfa :: MapInfo -> (Maybe Int, M.Map Coord Int)
runSpfa MapInfo {miStartEnd = (startCoord, endCoord), miGraph, miDist} =
  runState (spfaWith (PQ.singleton startCoord 0)) (M.singleton startCoord 0)
  where
    spfaWith q0 =
      case PQ.minView q0 of
        Nothing ->
          gets (M.!? endCoord)
        Just (u PQ.:-> distU, q1) -> do
          performEnqs <- forM (M.keys (miGraph M.! u)) $ \v -> do
            let distV' = distU + fromJust (getDist miDist (u, v))
            mDistV <- gets (M.!? v)
            if maybe True (distV' <) mDistV
              then do
                modify (M.insert v distV')
                pure
                  (PQ.alter
                     (\case
                        Nothing -> Just distV'
                        Just distV -> Just (min distV distV'))
                     v)
              else pure id
          spfaWith $ appEndo (foldMap Endo performEnqs) q1

simplifyMapInfo :: MapInfo -> MapInfo
simplifyMapInfo mi@MapInfo {miGraph} = simplifyMapInfoAux mi $ PQ.fromList do
  (coord, cs) <- M.toList miGraph
  let deg = M.size cs
  guard $ deg <= 2
  pure (coord PQ.:-> deg)

-- TODO: we should probably have an util module that deal with this kind of things for undirected graphs.
getDist :: M.Map (MinMax Coord) Int -> (Coord, Coord) -> Maybe Int
getDist m p = m M.!? minMaxFromPair p

{-
  Basically the same simplification process as in day 18.
 -}
simplifyMapInfoAux :: MapInfo -> PQ.PSQ Coord Int -> MapInfo
simplifyMapInfoAux
  mi@MapInfo {miGraph, miDist, miStartEnd = (startCoord, endCoord)}
  q0 = case PQ.minView q0 of
    Nothing -> mi
    Just (c PQ.:-> deg, q1) ->
      if c == startCoord || c == endCoord
        then simplifyMapInfoAux mi q1
        else case deg of
          1 ->
            let [ ( c'
                    , Nothing {- it should never be the case that dead end leads to anywhere. -}
                    )
                  ] = M.toList (miGraph M.! c)
                miGraph' = M.adjust (M.delete c) c' $ M.delete c miGraph
                q2 = PQ.insert c' (M.size $ miGraph' M.! c') q1
             in simplifyMapInfoAux mi {miGraph = miGraph'} q2
          2 ->
            let [(c1, p1), (c2, p2)] = M.toList (miGraph M.! c)
                mOldDist = getDist miDist (c1, c2)
                newDist = fromJust (getDist miDist (c, c1)) + fromJust (getDist miDist (c, c2))
                safeToPrune = case (p1, p2) of
                  (Nothing, Nothing) ->
                    case mOldDist of
                      Nothing -> True
                      Just oldDist -> newDist <= oldDist
                  _ -> False
                miGraph' =
                  M.adjust (M.insert c1 Nothing . M.delete c) c2 $
                    M.adjust (M.insert c2 Nothing . M.delete c) c1 $
                      M.delete c miGraph
                miDist' =
                  -- probably not worth removing old ones
                  M.insert (minMaxFromPair (c1, c2)) newDist miDist
                enqueue cx = PQ.insert cx (M.size $ miGraph' M.! cx)
                q2 = enqueue c1 . enqueue c2 $ q1
             in if safeToPrune
                  then simplifyMapInfoAux mi {miGraph = miGraph', miDist = miDist'} q2
                  else simplifyMapInfoAux mi q1
          _
            | deg >= 3 ->
              -- meaning all deg 1 and 2 are done.
              mi
          _ -> unreachable

type SearchState2 = (Coord, Int) -- current coord and recursion level

bfs :: MapInfo -> PQ.PSQ SearchState2 Int -> S.Set SearchState2 -> Int
bfs
  mi@MapInfo
    { miStartEnd = (_startCoord, endCoord)
    , miGraph
    , miDist
    }
  q0
  discovered = case PQ.minView q0 of
    Nothing -> error "queue exhausted"
    Just (st@(coord, level) PQ.:-> stepCount, q1) ->
      if st == (endCoord, 0)
        then stepCount
        else
          let nexts = do
                (coord', mPortal) <- M.toList (miGraph M.! coord)
                let stepCount' = stepCount + (miDist M.! minMaxFromPair (coord, coord'))
                    level' = case mPortal of
                      Nothing -> level
                      Just (_, _, PsInner) -> level + 1
                      Just (_, _, PsOuter) -> level - 1
                    st' = (coord', level')
                guard $ level' >= 0 && S.notMember st' discovered
                pure (st', stepCount')
              discovered' = S.union discovered $ S.fromList (fmap fst nexts)
              q2 = foldr enqueue q1 nexts
                where
                  enqueue (st', stepCount') curQ =
                    PQ.alter
                      (\case
                         Nothing -> Just stepCount'
                         Just sc -> Just (min sc stepCount'))
                      st'
                      curQ
           in bfs mi q2 discovered'

instance Solution Day20 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (extraOps, rawInput) <- consumeExtraLeadingLines <$> getInputS
    let xs = lines rawInput
        rows = length xs
        cols = length (head xs)
        runPart1 = maybe True ("part1" `elem`) extraOps
        runPart2 = maybe True ("part2" `elem`) extraOps
        rawFloor :: Arr.Array Coord Char
        rawFloor = Arr.array
          ((0, 0), (rows -1, cols -1))
          do
            (r, rs) <- zip [0 ..] xs
            (c, x) <- zip [0 ..] rs
            pure ((r, c), x)
        parsed = parseMap rawFloor
        mi = mkMapInfo parsed
        mi'@MapInfo {miStartEnd = (startCoord, _)} =
          simplifyMapInfo mi
        (Just endDist, _) = runSpfa mi'
        debug = False
    when debug do
      debugMapInfo mi'
    when runPart1 do
      answerShow endDist
    when runPart2 do
      let initSt :: SearchState2
          initSt = (startCoord, 0)
      answerShow $
        bfs
          mi'
          (PQ.singleton initSt 0)
          (S.singleton initSt)
