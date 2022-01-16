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

module Javran.AdventOfCode.Y2017.Day24
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
import Text.ParserCombinators.ReadP hiding (count, many, get)

data Day24 deriving (Generic)

type Conn = (Int, Int)

connP :: ReadP Conn
connP = (,) <$> (decimal1P <* char '/') <*> decimal1P

dfs conns cur strength len = do
  let alts =  do
        (cPre@(u,v), conns') <- pick conns
        c@(l,_) <- if u == v then [cPre] else [cPre, (v,u)]
        guard $ l == cur
        pure (c, conns')
  case alts of
    [] -> pure (strength, len)
    _:_ -> do
      ((l,r), conns') <- alts
      dfs conns' r (strength + l + r) (len + 1)

instance Solution Day24 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    conns <- fmap (consumeOrDie connP) . lines <$> getInputS
    let solutions = dfs conns 0 0 0
    do
      answerShow $ fst $ maximumBy (comparing fst) solutions
    do
      answerShow $ fst $ maximumBy (comparing (\(st,len) -> (len, st))) solutions
