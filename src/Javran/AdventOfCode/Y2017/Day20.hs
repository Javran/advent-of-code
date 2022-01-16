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

module Javran.AdventOfCode.Y2017.Day20
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
import Linear.V3
import Linear.Vector
import Text.ParserCombinators.ReadP hiding (count, get, many)
import Javran.AdventOfCode.TestExtra

data Day20 deriving (Generic)

type Loc = Point V3 Int

type Pt = (Loc, V3 Int, V3 Int)

ptP :: ReadP Pt
ptP =
  (,,)
    <$> (char 'p' *> (P <$> vecP) <* string ", ")
    <*> (char 'v' *> vecP <* string ", ")
    <*> (char 'a' *> vecP)
  where
    intP = readS_to_P (reads @Int)
    vecP = between (string "=<") (char '>') do
      [a, b, c] <- intP `sepBy1` char ','
      pure $ V3 a b c

locAtTime :: Pt -> Int -> Loc
locAtTime (p, v, a) t = p .+^ (v ^* t) .+^ (a ^* ((t + 1) * t `quot` 2))

type System = (IM.IntMap Pt, Int)

step :: System -> System
step (m, t) = (IM.withoutKeys m (IS.fromList collisions), t')
  where
    t' = t + 1
    locs = M.fromListWith (<>) do
      (i, pt) <- IM.toList m
      pure (locAtTime pt t', [i])
    collisions =
      concatMap
        (\(_k, vs) -> case vs of
           [] -> unreachable
           [_] -> []
           _ -> vs)
        $ M.toList locs

instance Solution Day20 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (extraOps, rawInput) <- consumeExtra getInputS
    let xs = fmap (consumeOrDie ptP) . lines $ rawInput
        (runPart1 , runPart2 ) = shouldRun extraOps
    {-
      I have no idea how to do this correctly,
      my intuition says simulating that long enough we'll get the system to stablize,
      and thanks to the algebra we do have an efficient way to compute particle's
      location at any time.
     -}
    let tracedXs :: [(Int, Pt)]
        tracedXs = zip [0 :: Int ..] xs

    when runPart1 do
      let guess = 200000
          ys = fmap (second (\x -> manhattan 0 $ locAtTime x guess)) tracedXs
          (ans, _) = minimumBy (comparing snd) ys
      answerShow ans
    when runPart2 do
      let progression = iterate step (IM.fromList tracedXs, 0)
      answerShow $ last $ take 200 (fmap (IM.size . fst) progression)
