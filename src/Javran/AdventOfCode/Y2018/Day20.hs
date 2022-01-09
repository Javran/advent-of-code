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

module Javran.AdventOfCode.Y2018.Day20
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
import Shower

data Day20 deriving (Generic)

data Dir = N | W | S | E deriving (Show)

data Re
  = ReAtom Dir
  | ReSeq [Re]
  | ReAlt [Re]
  deriving (Show)

reP :: ReadP Re
reP = between (char '^') (char '$') reAltP
  where
    reAltP :: ReadP Re
    reAltP = ReAlt <$> (reSeqP `sepBy` char '|')

    {- Parses a regualar expr free of direct `|`s -}
    reSeqP :: ReadP Re
    reSeqP = ReSeq <$> (many (between (char '(') (char ')') reAltP <++ reAtomP))

    reAtomP :: ReadP Re
    reAtomP =
      foldl1
        (<++)
        [ ReAtom N <$ char 'N'
        , ReAtom W <$ char 'W'
        , ReAtom S <$ char 'S'
        , ReAtom E <$ char 'E'
        ]

instance Solution Day20 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    re <- consumeOrDie reP . head . lines <$> getInputS
    printer re
