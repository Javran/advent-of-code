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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2018.Day15
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day15 deriving (Generic)

{-
  Note that for this one *y goes first, then x*
  This is so that we have Ord instance being consistent with "reading order".
 -}
type Coord = (Int, Int)

{-
  storing mapping from a unit to its hitpoint.

  GameState = (<Hitpoints of Elves>, <Hitpoints of Goblins>)
 -}
type GameState = (M.Map Coord Int, M.Map Coord Int)

type Graph = M.Map Coord [Coord]

parseFromRaw :: [String] -> (Graph, GameState)
parseFromRaw raw =
  ( g
  , ( M.fromList $ concat elves
    , M.fromList $ concat goblins
    )
  )
  where
    g = M.fromListWith (<>) do
      coord@(y, x) <- opens
      coord' <-
        -- explicit listing to make sure it's in reading order.
        [(y -1, x), (y, x -1), (y, x + 1), (y + 1, x)]
      guard $ S.member coord' opensSet
      pure (coord, [coord'])
    (elves, goblins) = unzip combatUnits
    opensSet = S.fromList opens
    (opens, combatUnits) = unzip do
      (y, rs) <- zip [0 ..] raw
      (x, ch) <- zip [0 ..] rs
      guard $ ch `elem` ".EG"
      let coord = (y, x)
          em = [(coord, 200) | ch == 'E']
          gm = [(coord, 200) | ch == 'G']
      pure (coord, (em, gm))

pprGame :: Graph -> GameState -> IO ()
pprGame g (elves, goblins) = do
  let Just (MinMax2D ((minY, maxY), (minX, maxX))) =
        foldMap (Just . minMax2D) $ M.keys g
  forM_ [minY - 1 .. maxY + 1] \y -> do
    let render x
          | Just _ <- elves M.!? coord = "E"
          | Just _ <- goblins M.!? coord = "G"
          | Just _ <- g M.!? coord = " "
          | otherwise = "█"
          where
            coord = (y, x)
    putStrLn $ concatMap render [minX - 1 .. maxX + 1]

instance Solution Day15 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- lines <$> getInputS
    let (g, gs) = parseFromRaw xs
    pprGame g gs
