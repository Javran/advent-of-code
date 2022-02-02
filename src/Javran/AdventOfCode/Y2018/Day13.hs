{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2018.Day13
  (
  )
where

import Control.Monad
import Control.Monad.State.Strict
import Data.List
import qualified Data.Map.Strict as M
import Data.Void
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.TestExtra

data Day13 deriving (Generic)

{-
  coord system:
  o ----- > x
  |
  |
  V
  y

 -}

data Dir = U | L | D | R deriving (Show, Ord, Eq, Enum)

applyDir :: Dir -> Coord -> Coord
applyDir = \case
  U -> second pred
  L -> first pred
  D -> second succ
  R -> first succ

turnLeft, turnRight :: Dir -> Dir
(turnLeft, turnRight) = (\d -> ds !! (fromEnum d + 1), \d -> ds !! (fromEnum d + 3))
  where
    ds = cycle [U .. R]

type Coord = (Int, Int) -- x and y

data Sym
  = SCurve Bool {- False: '\', True: '/' -}
  | SCart Dir
  | SIntersect
  | SPipe Bool {- False: '-', True: '|' -}
  deriving (Show)

data MapInfo = MapInfo
  { miGraph :: M.Map Coord [Coord]
  , miCurves :: M.Map Coord Bool
  , miCarts :: [(Coord, Dir)]
  }
  deriving (Show)

data CartState = CartState
  { csLoc :: Coord -- current location
  , csDir :: Dir -- current direction
  , csTurnOpt :: [Dir -> Dir] -- infinite list of turning decisions, to be used on intersections
  }

parseSym :: Char -> Maybe Sym
parseSym = \case
  ' ' -> Nothing
  '\\' -> Just $ SCurve False
  '/' -> Just $ SCurve True
  '+' -> Just SIntersect
  '^' -> Just $ SCart U
  '<' -> Just $ SCart L
  'v' -> Just $ SCart D
  '>' -> Just $ SCart R
  '-' -> Just $ SPipe False
  '|' -> Just $ SPipe True
  s -> error $ "Unknown: " <> show s

parseFromRaw :: [String] -> MapInfo
parseFromRaw raw = MapInfo {miGraph, miCurves, miCarts}
  where
    miCurves = M.fromList do
      (coord, SCurve v) <- M.toList syms
      pure (coord, v)
    miCarts = do
      (coord, SCart d) <- M.toList syms
      pure (coord, d)
    miGraph = M.fromList do
      (coord, sym) <- M.toList syms
      let -- checks connectivity from current coord to a neighborhood.
          conn :: Dir -> Maybe Coord
          conn dir = do
            let coord' = applyDir dir coord
                expectVertical = dir `elem` [U, D]
                tt = Just coord'
            s' <- syms M.!? coord'
            case s' of
              SCurve _ -> tt
              SIntersect -> tt
              SCart d' -> do
                guard
                  if expectVertical
                    then d' `elem` [U, D]
                    else d' `elem` [L, R]
                tt
              SPipe vert -> do
                guard
                  if expectVertical
                    then vert
                    else not vert
                tt
      let connInfo@[connU, connL, connD, connR] = fmap conn [U, L, D, R]
          errConn =
            error $
              "unexpected connectivity on " <> show (coord, sym) <> " : " <> show connInfo
          checkAndReturn conns = case sequence conns of
            Just ds -> pure (coord, ds)
            Nothing -> errConn
          handlePipe vert =
            {-
              pipes only care about its own directions,
              otherwise cases with curves would be tricky,

              e.g.
                |
                \---
              --a--
                /---
                |

              From a's respective, it might seem the `\` and `/` could be connected
              while actually they are no, if a is `-`.
              We could go beyond to preprocess those curves first to fix them
              to a specific connectivity, but I don't see much benefit doing
              an extra round just for this, so let's settle for just looking at
              our direction of interest only.
             -}
            checkAndReturn (if vert then [connU, connD] else [connL, connR])
          handleCurve connAlt0 connAlt1 = do
            let [alt0, alt1] = fmap sequence [connAlt0, connAlt1]
            -- make sure it's mutual exclusive.
            case (alt0, alt1) of
              (Nothing, Just ds) -> pure (coord, ds)
              (Just ds, Nothing) -> pure (coord, ds)
              (_, _) -> errConn
      case sym of
        SCurve False ->
          -- '\' connects either U,R or L,D,
          handleCurve [connU, connR] [connL, connD]
        SCurve True ->
          -- '/' connects either U,L or D,R, make sure it's mutual exclusive.
          handleCurve [connU, connL] [connD, connR]
        SIntersect ->
          checkAndReturn connInfo
        SPipe vert -> handlePipe vert
        SCart dir -> handlePipe (dir `elem` [U, D])
    syms :: M.Map Coord Sym
    syms = M.fromList do
      (y, rs) <- zip [0 ..] raw
      (x, ch) <- zip [0 ..] rs
      Just s <- pure $ parseSym ch
      pure ((x, y), s)

tickCart :: MapInfo -> CartState -> CartState
tickCart
  MapInfo {miGraph, miCurves}
  csOld@CartState {csLoc = oldLoc, csDir = oldDir, csTurnOpt = oldTurnOpt}
    | atIntersect =
      let f : csTurnOpt = oldTurnOpt
       in CartState {csLoc, csDir = f oldDir, csTurnOpt}
    | Just curveType <- atCurve =
      let csDir =
            {-
              Determines how cart's direction is changed when it hits a curve:
              oldDir is the direction that we are moving in.
             -}
            case curveType of
              False ->
                -- '\'
                case oldDir of
                  U -> L
                  D -> R
                  L -> U
                  R -> D
              True ->
                -- '/'
                case oldDir of
                  U -> R
                  D -> L
                  L -> D
                  R -> U
       in CartState {csLoc, csTurnOpt = oldTurnOpt, csDir}
    | otherwise = csOld {csLoc}
    where
      atCurve :: Maybe Bool
      atCurve = miCurves M.!? csLoc
      atIntersect = case miGraph M.! csLoc of
        [_, _, _, _] -> True
        _ -> False
      csLoc = case miGraph M.!? newLoc of
        Just _ -> newLoc
        Nothing -> error $ "Failed: " <> show (oldLoc, oldDir)
        where
          newLoc = applyDir oldDir oldLoc

-- carts are identified by its current location.
type CartSimState = M.Map Coord CartState

type CartSim = StateT CartSimState (Either Coord)

{-
  TODO: it might be possible that we recover from an error and continue
  by using continuation, but I haven't find a way to do that yet.
  (just need to investigate a bit, it might be too complicated to actually do it).
 -}
tick :: Bool -> MapInfo -> CartSim ()
tick stopOnConflict mi = do
  {-
    We need to make sure y goes first, then x.
    One alternative is that we can make Coord a newtype and customize its Ord
    instance. This way we can just use toAscList.
    But I feel that's too much effort for too little benefit.
   -}
  carts <- gets (sortOn (\((x, y), _) -> (y, x)) . M.toList)
  forM_ carts $ \(cartLoc, cartState) -> do
    {-
      As in part 2 we are asked to continue the simulate even when carts crash,
      a cart might not exist anymore when we get to it in current tick.
     -}
    exist <- gets (M.member cartLoc)
    when exist do
      modify (M.delete cartLoc)
      let cartState'@CartState {csLoc = newLoc} = tickCart mi cartState
      v <- gets (M.!? newLoc)
      case v of
        Just _ -> do
          when stopOnConflict do
            lift $ Left newLoc
          modify (M.delete newLoc)
        Nothing ->
          modify (M.insert newLoc cartState')
  {-
    Note that this is outside of the for loop,
    as the puzzle asks us to check after a tick is done,
    meaning even if there's only one cart remaining,
    we should tick it first before reporting its location.
   -}
  sz <- gets M.size
  when (sz == 1) do
    ~[(c, _)] <- gets M.toList
    lift $ Left c

simulate :: Bool -> MapInfo -> (Int, Int)
simulate stopOnConflict mi =
  case evalStateT (forever $ tick stopOnConflict mi) initSt of
    Left r -> r
    Right v -> absurd v
  where
    initSt =
      M.fromList
        (fmap
           (\(coord, dir) ->
              ( coord
              , CartState
                  { csLoc = coord
                  , csDir = dir
                  , csTurnOpt = cycle [turnLeft, id, turnRight]
                  }
              ))
           (miCarts mi))

instance Solution Day13 where
  solutionRun _ SolutionContext {getInputS, answerS} = do
    (ex, rawInput) <- consumeExtra getInputS
    let xs = lines rawInput
        mi = parseFromRaw xs
        (runPart1, runPart2) = shouldRun ex
    when runPart1 do
      let (x, y) = simulate True mi
      answerS $ show x <> "," <> show y
    when runPart2 do
      let (x, y) = simulate False mi
      answerS $ show x <> "," <> show y
