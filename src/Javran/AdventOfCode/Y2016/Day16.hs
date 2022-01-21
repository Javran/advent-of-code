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

module Javran.AdventOfCode.Y2016.Day16
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
import Javran.AdventOfCode.TestExtra (consumeExtra)
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day16 deriving (Generic)

type DragonSeq = (Int, Int -> Bool)

bitP :: ReadP Bool
bitP = (False <$ char '0') <++ (True <$ char '1')

ppr :: DragonSeq -> String
ppr (l, getAt) = fmap (bool '0' '1' . getAt) [0 .. l -1]

gen :: DragonSeq -> DragonSeq
gen (l, getAt) = (l * 2 + 1, f)
  where
    f i = case compare i l of
      LT -> getAt i
      EQ -> False
      GT -> not (getAt (2 * l - i))

instance Solution Day16 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (extraOpts, rawInput) <- consumeExtra getInputS
    let xs = V.fromList . consumeOrDie (many1 bitP) . head . lines $ rawInput
        initSeq :: DragonSeq
        initSeq = (V.length xs, (xs V.!))
        fillLen = case extraOpts of
          Nothing -> 272
          Just ~[raw] -> read raw
    mapM_ (putStrLn . ppr) (take 4 (iterate gen initSeq))
