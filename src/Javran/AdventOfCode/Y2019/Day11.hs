{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2019.Day11
  (
  )
where

import Control.Monad
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Bool
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Vector.Unboxed as VU
import GHC.Generics (Generic)
import Javran.AdventOfCode.ColorfulTerminal hiding (Color (..))
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2019.IntCode
import Linear.Affine
import Linear.V2
import qualified System.Console.Terminfo.Color as TermColor

data Day11 deriving (Generic)

type Color = Bool

pattern Black, White :: Color
pattern Black = False
pattern White = True

{-# COMPLETE Black, White #-}

{-
  Coordinate system: y is row, top-bottom, x is col, left-right, both 0-based.

  This setup results in a left-handed system,
  so `perp` below actually rotates the vector clockwise
  despite what document says.
 -}
dirs :: [V2 Int]
dirs = take 4 $ iterate perp (V2 0 (-1))

dirChars :: [] Char
dirChars = "^>v<"

type Robot =
  ( Point V2 Int -- where it is
  , Int -- [0..3], where it is facing (index into dirs)
  )

type St = (Robot, M.Map (Point V2 Int) Color)

paintState :: OutputMethod -> St -> IO ()
paintState outMethod ((roboLoc, roboTurn), m) = do
  let touched :: [Point V2 Int]
      touched = roboLoc : M.keys m
      Just (MinMax2D ((minX, maxX), (minY, maxY))) =
        foldMap (\(P (V2 x y)) -> Just (minMax2D (x, y))) touched
      getColor :: Point V2 Int -> Color
      getColor loc = fromMaybe Black (m M.!? loc)
      charAtLoc :: Bool -> Point V2 Int -> Char
      charAtLoc fancy loc =
        if roboLoc == loc
          then dirChars !! roboTurn
          else case getColor loc of
            Black -> bool '.' ' ' fancy
            White -> bool '#' '█' fancy
      infoLine =
        "Modified region: "
          <> "X(col): "
          <> show (minX, maxX)
          <> ", Y(row): "
          <> show (minY, maxY)
      outputInBasicMode (outputerLn :: String -> IO ()) = do
        outputerLn infoLine
        forM_ [minY .. maxY] $ \y -> do
          let rowLocs = fmap (\x -> P (V2 x y)) [minX .. maxX]
          outputerLn (charAtLoc False <$> rowLocs)

  case outMethod of
    OutputForTest outputer ->
      outputInBasicMode outputer
    OutputBasicTerm ->
      outputInBasicMode putStrLn
    OutputColorTerm
      ColorfulTerminal
        { setForeground = withFg
        , setBackground = withBg
        , runTermOut
        } -> do
        putStrLn infoLine
        let textAtLoc loc =
              if roboLoc == loc
                then
                  if getColor loc == White
                    then withBg TermColor.Cyan $ withFg TermColor.Black t
                    else withFg TermColor.Magenta t
                else withFg TermColor.Cyan t
              where
                t = termText [charAtLoc True loc]
        forM_ [minY -1 .. maxY + 1] $ \y -> do
          let rowLocs = fmap (\x -> P (V2 x y)) [minX -1 .. maxX + 1]
          runTermOut (foldMap textAtLoc rowLocs)
          putStrLn ""

performPainting :: (St -> IO ()) -> Result (VU.Vector Int) -> StateT St IO ()
performPainting painter = \case
  Done {} -> pure ()
  r@NeedInput {} -> do
    color <- getCurColor
    ([rawPaintColor, rawTurn], k2) <-
      liftIO $
        communicate [if color == Black then 0 else 1] 2 (pure r)
    (loc, turn) <- gets fst
    let paintColor = case rawPaintColor of
          0 -> Black
          1 -> White
          _ -> errInvalid
        turn' = case rawTurn of
          0 -> (turn + 3) `rem` 4
          1 -> (turn + 1) `rem` 4
          _ -> errInvalid
        loc' = loc .+^ (dirs !! turn')
    modify
      (bimap
         (-- turn and move forward
          const (loc', turn'))
         (-- paint on current location
          M.insert loc paintColor))
    get >>= \s -> liftIO $ painter s -- for debugging
    r3 <- liftIO k2
    performPainting painter r3
  SentOutput {} -> unexpected
  where
    getCurColor = gets (\((loc, _), m) -> fromMaybe Black (m M.!? loc))
    unexpected = error "unexpected state"

instance Solution Day11 where
  solutionRun _ SolutionContext {getInputS, answerShow, answerS, terminal} = do
    xs <- fmap (read @Int) . splitOn "," . head . lines <$> getInputS
    let mem = VU.fromList xs
        runWithInitialMap m = do
          prog <- startProgram mem
          execStateT
            (performPainting (const (pure ())) prog)
            ((P (V2 0 0), 0), m)

    do
      (_roboSt, m) <- runWithInitialMap M.empty
      answerShow (M.size m)
    do
      st <- runWithInitialMap $ M.singleton (P $ V2 0 0) White
      paintState
        (getOutputMethod answerS terminal)
        st
