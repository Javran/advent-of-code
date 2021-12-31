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

type Rules = M.Map [Bool] Bool

step :: Rules -> [Bool] -> [Bool]
step rules st = fmap (fromMaybe False . (rules M.!?)) $ divvy 5 1 (fff <> st <> fff)
  where
    fff = replicate 3 False

instance Solution Day12 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    [[initStRaw], rulesRaw] <- splitOn [""] . lines <$> getInputS
    let initSt = consumeOrDie initStateP initStRaw
        rules =
          M.fromListWith (error "rule conflict") $
            fmap (consumeOrDie ruleP) rulesRaw
        progression = iterate (step rules) initSt

    do
      let ans = sum $ catMaybes $ zipWith (\v i -> if v then Just i else Nothing) (progression !! 20) [-20 :: Int ..]
      answerShow ans
