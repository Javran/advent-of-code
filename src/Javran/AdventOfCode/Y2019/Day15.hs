{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Javran.AdventOfCode.Y2019.Day15
  (
  )
where

import Control.Monad
import Control.Monad.State.Strict
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Javran.AdventOfCode.ColorfulTerminal
import Javran.AdventOfCode.GridSystem.RowThenCol.Nwse
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2019.IntCode

data Day15 deriving (Generic)

{-
  Assumptions for this puzzle:

  - Robot stands on an empty cell that is not oxygen system, at the beginning.
  - Floor does not change as droid moves.
  - Space is closed, we won't eventually stuck into exploring unbounded spaces.
  - The space is reasonably small that pathfinding simply with BFS
    is good enough for this puzzle.

 -}

data Cell = CEmpty | CWall deriving (Show, Eq)

type FloorInfo = M.Map Coord Cell

type CoordSet = S.Set Coord

data SystemState = SystemState
  { -- | known part of the floor
    ssFloorInfo :: FloorInfo
  , -- | droid location
    ssDroid :: Coord
  , -- | unknown coords adjacent to known floor cells.
    ssUnknowns :: CoordSet
  , -- | suspended program
    ssProg :: IO (Result ())
  , -- | location of the oxygen system.
    ssOxygenSys :: Maybe Coord
  }

type PathPlanInfo = (CoordSet, Coord)

data CoordRenderInfo
  = CrIsKnown Cell
  | CrIsUnknown
  | CrIsOrigin
  | CrIsDroid
  | CrIsOxygenSys

showSystemState :: OutputMethod -> Maybe PathPlanInfo -> SystemState -> IO ()
showSystemState
  om
  mPlan
  SystemState
    { ssFloorInfo
    , ssUnknowns
    , ssDroid
    , ssOxygenSys
    } = case om of
    Left _ -> pure ()
    Right t -> do
      threadDelay (1000 * 20)
      let Just (MinMax2D (rangeR@(minR, maxR), rangeC@(minC, maxC))) =
            foldMap (Just . minMax2D) $
              M.keys ssFloorInfo <> S.toList ssUnknowns
          (runTermOut, renderCoord) = case t of
            BasicTerm rto ->
              ( rto
              , \coord -> case infoAtCoord coord of
                  Nothing -> termText "  "
                  Just i -> case i of
                    CrIsKnown CEmpty -> termText ".."
                    CrIsKnown CWall -> termText "##"
                    CrIsUnknown -> termText "??"
                    CrIsOrigin -> termText "><"
                    CrIsDroid -> termText "Dr"
                    CrIsOxygenSys -> termText "O2"
              )
            ColorTerm
              ColorfulTerminal
                { runTermOut = rto
                , setForeground = setFg
                , setBackground = setBg
                } ->
                ( rto
                , \coord -> case infoAtCoord coord of
                    Nothing -> termText "  "
                    Just i -> case i of
                      CrIsKnown CEmpty ->
                        case mPlan of
                          Just (ps, _)
                            | S.member coord ps ->
                              setBg Magenta $ termText "  "
                          _ -> setBg Cyan $ termText "  "
                      CrIsKnown CWall -> setFg White $ termText "██"
                      CrIsUnknown ->
                        case mPlan of
                          Just (_, lastCoord)
                            | coord == lastCoord ->
                              setBg Magenta $ setFg White $ termText "??"
                          _ ->
                            setFg Red $ termText "??"
                      CrIsOrigin -> setBg Cyan $ setFg Black $ termText "><"
                      CrIsDroid -> setBg Magenta $ setFg White $ termText "Dr"
                      CrIsOxygenSys -> setBg Cyan $ setFg Black $ termText "O2"
                )
          infoAtCoord coord
            | coord == ssDroid = Just CrIsDroid
            | Just coord == ssOxygenSys = Just CrIsOxygenSys
            | coord == (0, 0) = Just CrIsOrigin
            | Just c <- ssFloorInfo M.!? coord = Just $ CrIsKnown c
            | S.member coord ssUnknowns = Just CrIsUnknown
            | otherwise = Nothing
      putStrLn $
        "Discovered region: rows: " <> show rangeR <> ", cols: " <> show rangeC
      forM_ [minR -1 .. maxR + 1] $ \row -> do
        let rowOut = mconcat (fmap (renderCoord . (row,)) [minC -2 .. maxC + 2])
        runTermOut (rowOut <> termText "\n")

{-
  Performs a BFS to find path to any of the given unknown coords.

  - Returns a *reversed* list of path
  - Start location is given as the initial singleton queue.
  - The path is only allowed to go into any of the following:
    + A unknown coord. In which case it must be the last step
      (therefore the first element of the reversed path)
      and this function returns immediately.
    + a CEmpty

 -}
findPathToAnyTarget
  :: FloorInfo
  -> CoordSet
  -> Seq.Seq (Coord, [Dir])
  -> CoordSet
  -> Maybe (Coord, [Dir])
findPathToAnyTarget floorInfo unknowns q discovered = case q of
  Seq.Empty -> Nothing
  (coord, revPath) Seq.:<| q' ->
    if S.member coord unknowns
      then Just (coord, revPath)
      else
        let nextCoords = do
              d <- allDirs
              let coord' = applyDir d coord
              guard $
                S.member coord' unknowns
                  || M.lookup coord' floorInfo == Just CEmpty
              guard $ S.notMember coord' discovered
              pure (coord', d : revPath)
         in findPathToAnyTarget
              floorInfo
              unknowns
              (q' <> Seq.fromList nextCoords)
              (S.union discovered (S.fromList $ fmap fst nextCoords))

explore :: OutputMethod -> StateT SystemState IO ()
explore om = do
  {-
    Find path to the closest unknown, from current droid location.
    compute path xs, terminate if the set is empty.
   -}
  ss@SystemState {ssFloorInfo, ssDroid, ssUnknowns} <- get
  liftIO $ showSystemState om Nothing ss
  let mExploreTarget =
        findPathToAnyTarget
          ssFloorInfo
          ssUnknowns
          (Seq.singleton (ssDroid, []))
          (S.singleton ssDroid)
  case mExploreTarget of
    Nothing -> pure ()
    Just (_, []) -> unreachable
    Just (_targetCoord, allStepsRev@(lastStep : safeStepsRev)) -> do
      ssCur@SystemState {ssDroid = dCoord} <- get
      let pathPlan = tail $ scanl (flip applyDir) dCoord (reverse allStepsRev)
      {-
        now we have a plan going from ssDroid to targetCoord,
        in which case all steps except for the last one
        are safe to perform
       -}
      forM_ (zip (reverse safeStepsRev) (tails pathPlan)) $ \(d, plan) -> do
        ss'' <- get
        liftIO $ showSystemState om (Just (S.fromList (init plan), last plan)) ss''
        True <- execCommand d
        pure ()
      -- Perform last step.
      _ <- execCommand lastStep
      liftIO $ showSystemState om (Just (S.empty, last pathPlan)) ssCur
      explore om
  where
    -- performs appropriate updates to ssFloorInfo and ssUnknowns
    updateInfo coord = \case
      CWall ->
        -- nothing new to expand to if we are hitting a wall.
        modify
          (\ss ->
             ss
               { ssUnknowns = S.delete coord (ssUnknowns ss)
               , ssFloorInfo = M.insert coord CWall (ssFloorInfo ss)
               })
      CEmpty -> do
        SystemState {ssUnknowns, ssFloorInfo} <- get
        let ssFloorInfo' = M.insert coord CEmpty ssFloorInfo
            expandCoords = S.fromList do
              {-
                learning that a cell is empty puts its neighborhood unknown cells
                into the set of unknown coords for later exploration.
               -}
              c' <- udlrOfCoord coord
              guard $ M.notMember c' ssFloorInfo
              pure c'
            ssUnknowns' = S.union expandCoords $ S.delete coord ssUnknowns
        modify (\ss -> ss {ssUnknowns = ssUnknowns', ssFloorInfo = ssFloorInfo'})

    execCommand dir = do
      let cmd = case dir of
            N -> 1
            S -> 2
            W -> 3
            E -> 4
      SystemState {ssProg, ssDroid} <- get
      let droidCoord' = applyDir dir ssDroid
      ([reply], prog') <- liftIO $ communicate [cmd] 1 ssProg
      modify (\ss -> ss {ssProg = prog'})
      case reply of
        0 -> do
          -- hitting a wall.
          updateInfo droidCoord' CWall
          pure False
        1 -> do
          -- empty
          modify (\ss -> ss {ssDroid = droidCoord'})
          updateInfo droidCoord' CEmpty
          pure True
        2 -> do
          modify (\ss -> ss {ssDroid = droidCoord'})
          -- also oxygen system located
          modify (\ss -> ss {ssOxygenSys = Just droidCoord'})
          updateInfo droidCoord' CEmpty
          pure True
        _ -> error $ "invalid reply: " <> show reply

{-
  BFS for max depth for part2.

  We could have extended findPathToAnyTarget to keep track of the max depth,
  But I think it's better to have this process separated to not tangle things too much.

 -}
bfsForOxygen
  :: FloorInfo
  -> M.Map Coord Int
  -> CoordSet
  -> Seq.Seq (Coord, Int)
  -> M.Map Coord Int
bfsForOxygen floorInfo acc discovered = \case
  Seq.Empty -> acc
  ((coord, depth) Seq.:<| q) ->
    let nextCoords = do
          c' <- udlrOfCoord coord
          Just CEmpty <- pure (floorInfo M.!? c')
          guard $ S.notMember c' discovered
          pure (c', depth + 1)
     in bfsForOxygen
          floorInfo
          (M.insert coord depth acc)
          (S.union discovered $ S.fromList (fmap fst nextCoords))
          (q <> Seq.fromList nextCoords)

instance Solution Day15 where
  solutionRun _ SolutionContext {getInputS, answerShow, answerS, terminal} = do
    xs <- parseCodeOrDie <$> getInputS
    let initSys =
          SystemState
            { ssFloorInfo = M.singleton (0, 0) CEmpty
            , ssDroid = (0, 0)
            , ssUnknowns = S.fromList (udlrOfCoord (0, 0))
            , ssProg = void <$> startProgramFromFoldable xs
            , ssOxygenSys = Nothing
            }
        outputMethod = getOutputMethod answerS terminal
    SystemState {ssFloorInfo, ssOxygenSys = Just locOxy} <-
      execStateT (explore outputMethod) initSys
    let Just (MinMax2D ((minR, maxR), (minC, maxC))) =
          foldMap (Just . minMax2D) $
            M.keys ssFloorInfo
        display = False
    when display $
      forM_ [minR .. maxR] $ \r -> do
        let render c =
              if
                  | (r, c) == (0, 0) -> "><"
                  | (r, c) == locOxy -> "OX"
                  | otherwise -> case ssFloorInfo M.!? (r, c) of
                    Nothing -> "??"
                    Just CWall -> "##"
                    Just CEmpty -> "  "
            curRow = fmap render [minC .. maxC]
        putStrLn (concat curRow)
    let Just (_, revPath) =
          {-
            to find path to the oxygen system is to simply search for one pretending
            that oxygen location is the only unknown coord.
            (or, the only target that can terminate the search process)
          -}
          findPathToAnyTarget
            ssFloorInfo
            (S.singleton locOxy)
            (Seq.singleton ((0, 0), []))
            (S.singleton (0, 0))
    answerShow (length revPath)
    answerShow
      (maximum $
         bfsForOxygen
           ssFloorInfo
           M.empty
           (S.singleton locOxy)
           (Seq.singleton (locOxy, 0)))
