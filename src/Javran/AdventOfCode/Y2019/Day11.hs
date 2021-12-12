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
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Semigroup
import qualified Data.Vector.Unboxed as VU
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2019.IntCode
import Linear.Affine
import Linear.V2
import qualified System.Console.Terminfo as Term

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

type OutputMethod =
  Either
    (String -> IO ())
    Term.Terminal

paintState :: OutputMethod -> St -> IO ()
paintState outMethod ((roboLoc, roboTurn), m) = do
  let touched :: [Point V2 Int]
      touched = roboLoc : M.keys m
      Just ((Min minX, Max maxX), (Min minY, Max maxY)) =
        foldMap (\(P (V2 x y)) -> Just ((Min x, Max x), (Min y, Max y))) touched
      getColor :: Point V2 Int -> Color
      getColor loc = fromMaybe Black (m M.!? loc)
      colorfulTerm = do
        Right t <- pure outMethod
        Term.getCapability
          t
          ((,) <$> Term.withForegroundColor @Term.TermOutput
             <*> Term.withBackgroundColor @Term.TermOutput)
      charAtLoc :: Point V2 Int -> Char
      charAtLoc loc =
        if roboLoc == loc
          then dirChars !! roboTurn
          else case getColor loc of
            Black -> maybe '.' (const ' ') colorfulTerm
            White -> maybe '#' (const '█') colorfulTerm
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
          outputerLn (charAtLoc <$> rowLocs)

  case outMethod of
    Right term -> do
      case colorfulTerm of
        Nothing -> outputInBasicMode putStrLn
        Just (withFg, withBg) -> do
          putStrLn infoLine
          let textAtLoc loc =
                if roboLoc == loc
                  then
                    if getColor loc == White
                      then withBg Term.Cyan $ withFg Term.Black t
                      else withFg Term.Magenta t
                  else withFg Term.Cyan t
                where
                  t = Term.termText [charAtLoc loc]
          forM_ [minY -1 .. maxY + 1] $ \y -> do
            let rowLocs = fmap (\x -> P (V2 x y)) [minX -1 .. maxX + 1]
            Term.runTermOutput term (foldMap textAtLoc rowLocs)
            putStrLn ""
    Left outputer -> do
      outputInBasicMode outputer
  pure ()

performPainting :: (St -> IO ()) -> Result (VU.Vector Int) -> StateT St IO ()
performPainting painter = \case
  Done {} -> pure ()
  NeedInput k0 -> do
    color <- getCurColor
    SentOutput rawPaintColor k1 <- liftIO $ k0 (if color == Black then 0 else 1)
    SentOutput rawTurn k2 <- liftIO k1
    -- paint
    (loc, turn) <- gets fst
    let paintColor = case rawPaintColor of
          0 -> Black
          1 -> White
          _ -> errInvalid
    modify (second (M.insert loc paintColor))
    -- turn and move forward
    let turn' = case rawTurn of
          0 -> (turn + 3) `rem` 4
          1 -> (turn + 1) `rem` 4
          _ -> errInvalid
        loc' = loc .+^ (dirs !! turn')
    modify (first (const (loc', turn')))
    get >>= \s -> liftIO $ painter s
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
    do
      prog <- startProgram mem
      (_roboSt, m) <-
        execStateT
          (performPainting (const (pure ())) prog)
          ((P (V2 0 0), 0), M.empty)
      answerShow (M.size m)
    do
      prog <- startProgram mem
      st <-
        execStateT
          (performPainting (const (pure ())) prog)
          ((P (V2 0 0), 0), M.singleton (P $ V2 0 0) White)
      paintState
        (case terminal of
           Nothing -> Left answerS
           Just t -> Right t)
        st
