{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2019.Day17
  (
  )
where

import Control.Lens hiding (universe)
import Control.Monad
import Control.Monad.Writer.CPS
import Data.Bifunctor
import Data.Char
import Data.Either
import Data.List
import Data.List.Split hiding (sepBy)
import Data.Monoid
import qualified Data.Set as S
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2019.IntCode

data Day17 deriving (Generic)

type Coord = (Int, Int)

-- alters rotate counter-clockwise
data Dir
  = U
  | L
  | D
  | R
  deriving (Show, Enum, Bounded)

turnLeft :: Dir -> Dir
turnLeft d = cycle universe !! (fromEnum d + 1)

turnRight :: Dir -> Dir
turnRight d = cycle universe !! (fromEnum d + 3)

applyDir :: Dir -> Coord -> Coord
applyDir = \case
  U -> first pred
  D -> first succ
  L -> second pred
  R -> second succ

type Robot = (Dir, Coord)

data MapInfo = MapInfo
  { miScaffolds :: S.Set Coord
  , miRobot :: Robot
  }
  deriving (Show)

parseRawMap :: String -> MapInfo
parseRawMap rawMap = MapInfo {miScaffolds, miRobot}
  where
    (miScaffolds, Data.Monoid.Last (Just miRobot)) = mconcat do
      (r, row) <- zip [0 ..] (lines rawMap)
      (c, x) <- zip [0 ..] row
      -- We assume that map always starts with robot on scaffolds.
      guard $ x `notElem` ".X"
      let coord = (r, c)
      let mRobot = case x of
            '^' -> pure (U, coord)
            'v' -> pure (D, coord)
            '<' -> pure (L, coord)
            '>' -> pure (R, coord)
            _ -> Nothing
      pure (S.singleton coord, Data.Monoid.Last mRobot)

data Move
  = Forward Int
  | TurnLeft
  | TurnRight
  deriving (Eq)

instance Show Move where
  show = \case
    Forward v -> show v
    TurnLeft -> "L"
    TurnRight -> "R"

{-
  Completes one segment of the scaffolding line by an optional turn followed
  by moving forward.
 -}
nextMoves :: MapInfo -> Robot -> WriterT [Move] Maybe Robot
nextMoves MapInfo {miScaffolds} (dir, coord) = do
  let frontCoord = applyDir dir coord
      leftCoord = applyDir (turnLeft dir) coord
      rightCoord = applyDir (turnRight dir) coord
  {-
    If can't move forward, try left or right, `Nothing` if neither is available.
   -}
  dirMod <-
    if
        | S.member frontCoord miScaffolds ->
          pure id
        | S.member leftCoord miScaffolds -> do
          tell [TurnLeft]
          pure turnLeft
        | S.member rightCoord miScaffolds -> do
          tell [TurnRight]
          pure turnRight
        | otherwise -> lift Nothing
  let dir' = dirMod dir
      frontCoords = takeWhile (`S.member` miScaffolds) $ tail $ iterate (applyDir dir') coord
  tell [Forward (length frontCoords)]
  pure (dir', last frontCoords)

computeMoves :: MapInfo -> Robot -> [Move]
computeMoves mi miRobot =
  {-
    This is technically incorrect if droid starts facing the opposite direction of
    scaffolding. But so far that has never been the case.
   -}
  concat $
    unfoldr
      (\robot -> do
         (robot', w) <- runWriterT (nextMoves mi robot)
         pure (w, robot'))
      miRobot

{-
  A program after some replacement:
  - A Left element is a sequence of moves not translated into any subroutine
  - A Right element `Right x` is the result of replacing that sequence of movements with
    subroutine x.
 -}
type ReplacedProgram = [Either [Move] Char]

replaceWithMoveFn :: Char -> [Move] -> [Move] -> ReplacedProgram
replaceWithMoveFn fnName fnBody xs = concatMap handleChunk $ split (onSublist fnBody) xs
  where
    handleChunk ys
      | null ys = []
      | ys == fnBody = [Right fnName]
      | otherwise = [Left ys]

encodeMoves :: [Move] -> String
encodeMoves = intercalate "," . fmap show

type AllRoutines =
  ( [] Char -- the main routine
  , [(Char, [Move])]
  )

{-
  TODO: we could write a QuickCheck to verify that the rontines we got does exactly what
  the original one does.
 -}
breakIntoRoutines :: [] Char -> [Move] -> [] AllRoutines
breakIntoRoutines progNames xs = breakIntoRoutinesAux progNames [] [Left xs]

withinLengthLimit :: String -> Bool
withinLengthLimit = (<= 20) . length

breakIntoRoutinesAux
  :: [] Char
  -> [(Char, [Move])]
  -> ReplacedProgram
  -> [AllRoutines]
breakIntoRoutinesAux newProgNames progList xs0 = do
  let mx = findLeft xs0
  case mx of
    Nothing -> do
      -- all translated to main routine.
      let (_, mainProg) = partitionEithers xs0
      -- verify length limit for main routine
      guard $ withinLengthLimit $ intercalate "," (fmap (: []) mainProg)
      pure (mainProg, reverse progList)
    Just sub0 -> do
      -- get a new name or the name list is exhausted (in which case we fail)
      (progName : newProgNames') <- pure newProgNames
      progBody <- reverse . takeWhile (withinLengthLimit . encodeMoves) $ inits sub0
      let xs1 :: ReplacedProgram
          xs1 =
            concatMap
              (either
                 (replaceWithMoveFn progName progBody)
                 ((: []) . Right))
              xs0
      breakIntoRoutinesAux newProgNames' ((progName, progBody) : progList) xs1
  where
    findLeft ys = case partitionEithers ys of
      ([], _) -> Nothing
      (x : _, _) -> Just x

encodeVaccumRobotInput :: AllRoutines -> String
encodeVaccumRobotInput (mainRoutine, progList) =
  if length progList > 3
    then error "program list is too long"
    else
      let getAndEncode i = encodeMoves (maybe [] snd (progList ^? ix i))
       in unlines $
            intercalate "," (fmap (: []) mainRoutine) :
            fmap getAndEncode [0, 1, 2] <> ["n"]

instance Solution Day17 where
  solutionRun _ SolutionContext {getInputS, answerShow, answerS} = do
    (extraOps, rawInput) <- consumeExtraLeadingLines <$> getInputS
    case extraOps of
      Nothing -> do
        let xs = parseCodeOrDie rawInput
        (_, out) <- runProgram xs []
        let rawMap = fmap chr out
            mi@MapInfo {miScaffolds, miRobot} = parseRawMap rawMap
            intersections = do
              coord <- S.toList miScaffolds
              guard $ all (`S.member` miScaffolds) (udlrOfCoord coord)
              pure coord
        answerShow (sum $ fmap (uncurry (*)) intersections)
        let originalMainMoves = computeMoves mi miRobot
            allRoutines : _ = breakIntoRoutines "ABC" originalMainMoves
            ys = 2 : tail xs
        (_, out2) <- runProgram ys (fmap ord (encodeVaccumRobotInput allRoutines))
        answerShow (last out2)
      Just _ -> do
        let mi@MapInfo {miRobot} = parseRawMap rawInput
            moves = computeMoves mi miRobot
        answerS (encodeMoves moves)
        answerS (encodeVaccumRobotInput $ head $ breakIntoRoutines "ABC" moves)
