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
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day11 deriving (Generic)

type Color = Bool

pattern Black, White :: Color
pattern Black = False
pattern White = True

{-# COMPLETE Black, White #-}

dirs :: [V2 Int]
dirs =
  -- right?
  take 4 $ iterate perp (V2 0 (-1))

type Robot =
  ( Point V2 Int -- where it is
  , Int -- [0..3], where it is facing (index into dirs)
  )

performPainting :: Result (VU.Vector Int) -> StateT (Robot, M.Map (Point V2 Int) Color) IO ()
performPainting = \case
  Done {} -> pure ()
  NeedInput k0 -> do
    color <- getCurColor
    SentOutput rawPaintColor k1 <- liftIO $ k0 (if Black then 0 else 1)
    SentOutput rawTurn k2 <- liftIO k1
    let pprP (P (V2 x y)) = show (x, y)
        pprV (V2 x y) = show (x, y)
    -- paint
    (loc, turn) <- gets fst
    let paintColor = case rawPaintColor of
          0 -> Black
          1 -> White
          _ -> errInvalid
    modify (second (M.insert loc paintColor))
    -- turn and move forward
    let turn' = case rawTurn of
          0 -> (turn + 3) `mod` 4
          1 -> (turn + 1) `mod` 4
          _ -> errInvalid
        loc' = loc .+^ (dirs !! turn')
    modify (first (const (loc', turn')))

    liftIO $ putStrLn "===="
    forM_ [-10 .. 20] $ \y -> do
      rs <- forM [-5 .. 50] $ \x ->
        getColorAtLoc (P (V2 x y))
      let tr White = '#'
          tr Black = '.'
      liftIO $ putStrLn $ (fmap tr rs)
    liftIO $ putStrLn "===="
    r3 <- liftIO k2
    performPainting r3
  SentOutput {} -> unexpected
  where
    getColorAtLoc loc = gets (\(_, m) -> fromMaybe Black (m M.!? loc))
    getCurColor = gets (\((loc, _), m) -> fromMaybe Black (m M.!? loc))
    unexpected = error "unexpected state"

instance Solution Day11 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (read @Int) . splitOn "," . head . lines <$> getInputS
    let mem = VU.fromList xs
    prog <- startProgram mem
    (_, (_, m)) <- runStateT (performPainting prog) ((P (V2 0 0), 0), M.empty)
    answerShow (M.size m)
