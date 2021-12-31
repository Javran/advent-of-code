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

module Javran.AdventOfCode.Y2018.Day12
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
import Debug.Trace
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day12 deriving (Generic)

boolP :: ReadP Bool
boolP = (False <$ char '.') <++ (True <$ char '#')

initStateP :: ReadP [Bool]
initStateP = string "initial state: " *> many1 boolP

ruleP :: ReadP ([Bool], Bool)
ruleP = do
  lhs <- many1 boolP
  _ <- string " => "
  rhs <- boolP
  pure (lhs, rhs)

_pprBools :: [Bool] -> String
_pprBools = fmap (bool '.' '#')

_pprWorld :: World -> String
_pprWorld w = case IS.minView w of
  Nothing -> ""
  Just (minLoc, _) ->
    let maxLoc = IS.findMax w
     in do
          l <- [minLoc .. maxLoc]
          pure $ bool '.' '#' (IS.member l w)

type Rules = M.Map [Bool] Bool

{-
  TODO: for part 2, obviously simulation won't cut it,
  so I suspect there's an attractor.

  First step, let's use a sparse rep of the state,
  which should make the simulation scale a bit,
  then we can try to determine if there's an attractor.

  Update: it doesn't appear to be an attractor,
  as the min and max of World expands indefinitely.

  However, I notice for both example input and login input,
  the # of active elements in the World eventually goes
  to a fix amount and never change,
  so we can probably utilize this property somehow.

 -}

type World = IS.IntSet

step :: Rules -> World -> World
step rules w = case IS.minView w of
  Nothing -> w
  Just (minLoc, _) ->
    let maxLoc = IS.findMax w
     in IS.fromDistinctAscList do
          loc <- [minLoc -1 .. maxLoc + 1]
          {-
            we technically need to examine `minLoc - 2` and `maxLoc + 2`,
            in case the following is part of the rule:

            ....# => #
            #.... => #

            however, for both example and my login, we have the following
            as part of the rule:

            ....# => .
            #.... => .

            meaning we can cut padding to just 3.

            TODO: verify this assumption about the rule.

          -}

          let locView = fmap (`IS.member` w) [loc -2 .. loc + 2]
              after = fromMaybe False $ rules M.!? locView
          guard after
          pure loc

{-
  Normalizes the minimal location so that it is always 0.
 -}
rebase :: World -> (Int, World)
rebase w = case IS.minView w of
  Nothing -> (0, w)
  Just (minLoc, _) -> (minLoc, IS.map (subtract minLoc) w)

instance Solution Day12 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    [[initStRaw], rulesRaw] <- splitOn [""] . lines <$> getInputS
    let parsedSt = consumeOrDie initStateP initStRaw
        initSt =
          IS.fromDistinctAscList $
            catMaybes $ zipWith (\v i -> if v then Just i else Nothing) parsedSt [0 ..]
        rules =
          M.fromListWith (error "rule conflict") $
            fmap (consumeOrDie ruleP) rulesRaw
        progression = fmap rebase $ iterate (step rules) initSt
        zs =
          dropWhile (uncurry ((/=) `on` snd)) $ zip progression (tail progression)

    do
      let (offset, w) = progression !! 20
      answerShow (offset * IS.size w + (sum $ IS.toList w))
    print $ take 1 zs
