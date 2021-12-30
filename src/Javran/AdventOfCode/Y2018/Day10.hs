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
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2018.Day10
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
import Linear.Affine
import Linear.V2
import Linear.Vector
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day10 deriving (Generic)

type Pt = (Point V2 Int, V2 Int)

ptP :: ReadP Pt
ptP = do
  let intP = readS_to_P (reads @Int)
      pairP = skipSpaces *> ((,) <$> intP <*> (string ", " *> skipSpaces *> intP))
  _ <- string "position=<"
  (x, y) <- pairP
  _ <- string "> velocity=<"
  (vx, vy) <- pairP
  _ <- char '>'
  pure (P (V2 x y), V2 vx vy)

stepPt :: Int -> Pt -> Pt
stepPt n (pos, vel) = (pos .+^ (vel ^* n), vel)

getRange :: [Pt] -> MinMax2D Int Int
getRange = fromJust . foldMap (\(P (V2 x y), _vel) -> Just $ minMax2D (x, y))

getArea :: [Pt] -> Int
getArea pts = (maxX - minX + 1) * (maxY - minY + 1)
  where
    MinMax2D ((minX, maxX), (minY, maxY)) = getRange pts

{-
  This one is solved in some manual manner: it makes sense that
  we just need to find some time around a specific time t when
  the bounding rectangle of all points is the smallest.

  TODO:
  for actually solving this by algorithm, I plan to
  do an initial estimate of t, then
  we can probably utilize https://en.wikipedia.org/wiki/Ternary_search
  to find the peak.

 -}

instance Solution Day10 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (consumeOrDie ptP) . lines <$> getInputS
    let giantSteps =
          {-
            Take some giant steps (in this case, 1000 at a time) to find a
            potential range, we expect this area to go all the way down
            then up again, so we are looking for a window [a,b,c], where
            a >= b but b <= c, and we can use (a,c) as our initial range.

            This also implies that one has to be careful when picking the
            step length to not overshot.
           -}
          fmap (\n -> (n, getArea $ fmap (stepPt n) xs)) [0, 1000 ..]
        [(lowN, _), _, (highN, _)] : _ =
          dropWhile (\[_, (_, u), (_, v)] -> u > v) $ divvy 3 1 giantSteps

    print (lowN, highN)
    forM_ [10345] $ \n -> do
      print n
      let pts = fmap (stepPt n) xs
          ptsSet = S.fromList (fmap fst pts)
          MinMax2D rng@((minX, maxX), (minY, maxY)) = getRange pts
          display = False
      when display do
        forM_ [minY .. maxY] \y -> do
          let render x = if S.member (P (V2 x y)) ptsSet then "█" else " "
          putStrLn $ concatMap render [minX .. maxX] <> "|"
