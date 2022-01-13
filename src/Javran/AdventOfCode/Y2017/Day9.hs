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

module Javran.AdventOfCode.Y2017.Day9
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
import Control.Monad.Writer.CPS

data Day9 deriving (Generic)

data Thing = Group [Thing] | Garbage deriving Show

garbageP :: ReadP Thing
garbageP =
  Garbage
    <$ between
      (char '<')
      (char '>')
      (many $
         void (char '!' >> nextCharP) <++ void (satisfy (/= '>')))

groupP :: ReadP Thing
groupP = Group <$> between (char '{') (char '}') (thingP `sepBy` char ',')

thingP :: ReadP Thing
thingP = groupP <++ garbageP

countScore :: Int -> Thing -> Writer (Sum Int) ()
countScore myScore = \case
  Group xs -> do
    tell (Sum myScore)
    mapM_ (countScore (myScore +1)) xs
  Garbage -> pure ()

instance Solution Day9 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    v <- consumeOrDie (thingP <* char '\n') <$> getInputS
    let ((), Sum ans1) = runWriter $ countScore 1 v
    answerShow ans1
