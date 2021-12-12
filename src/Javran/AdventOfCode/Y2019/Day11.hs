{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2019.Day11
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Bool
import Data.Char
import Data.Either
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2019.IntCode
import Linear.Affine
import Linear.V2
import qualified System.Console.Terminfo as Term
import Text.ParserCombinators.ReadP hiding (count, get, many)

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

paintState :: Maybe Term.Terminal -> St -> IO ()
paintState mTerm ((roboLoc, roboTurn), m) = do
  let touched :: [Point V2 Int]
      touched = roboLoc : M.keys m
      Just ((Min minX, Max maxX), (Min minY, Max maxY)) =
        foldMap (\(P (V2 x y)) -> Just ((Min x, Max x), (Min y, Max y))) touched
      getColor :: Point V2 Int -> Color
      getColor loc = fromMaybe Black (m M.!? loc)
      colorfulTerm = do
        t <- mTerm
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

  putStrLn $
    "Modified region: " <> "X(col): "
      <> show (minX, maxX)
      <> ", Y(row): "
      <> show (minY, maxY)
  forM_ [minY -1 .. maxY + 1] $ \y -> do
    let rowLocs = fmap (\x -> P (V2 x y)) [minX -1 .. maxX + 1]
    case colorfulTerm of
      Just (withFg, withBg) | Just term <- mTerm -> do
        let textAtLoc loc =
              if roboLoc == loc
                then
                  if getColor loc == White
                    then withBg Term.Cyan $ withFg Term.Black t
                    else withFg Term.Magenta t
                else withFg Term.Cyan t
              where
                t = Term.termText [charAtLoc loc]
        Term.runTermOutput term (foldMap textAtLoc rowLocs)
        putStrLn ""
      _ -> putStrLn (charAtLoc <$> rowLocs)
  -- threadDelay (1000 * 30)
  pure ()

performPainting :: (St -> IO ()) -> Result (VU.Vector Int) -> StateT St IO ()
performPainting painter = \case
  Done {} -> pure ()
  NeedInput k0 -> do
    color <- getCurColor
    SentOutput rawPaintColor k1 <- liftIO $ k0 (if Black then 0 else 1)
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
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow, terminal} = do
    xs <- fmap (read @Int) . splitOn "," . head . lines <$> getInputS
    let mem = VU.fromList xs
    prog <- startProgram mem
    (_roboSt, m) <-
      execStateT
        (performPainting (paintState terminal) prog)
        ((P (V2 0 0), 0), M.empty)
    answerShow (M.size m)
