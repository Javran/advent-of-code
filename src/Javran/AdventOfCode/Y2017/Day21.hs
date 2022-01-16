{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
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

module Javran.AdventOfCode.Y2017.Day21
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Char
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Proxy
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import GHC.TypeNats
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day21 deriving (Generic)

data Grid (n :: Nat) = Grid Int

type Grid2 = Grid 2

type Grid3 = Grid 3

type Grid4 = Grid 4

type Coord = (Int, Int) -- row, col

encodeGrid :: KnownNat n => proxy n -> [[Bool]] -> Grid n
encodeGrid pSz xss =
  if sz * sz == length xs
    then Grid $ foldl' (\acc (i, v) -> if v then setBit acc i else acc) 0 (zip [0 ..] xs)
    else error "unexpected length"
  where
    sz = fromIntegral $ natVal pSz
    xs = concat xss

decodeGrid :: forall n. KnownNat n => Grid n -> [[Bool]]
decodeGrid (Grid v) = chunksOf sz $ fmap (testBit v) [0 .. sz * sz -1]
  where
    sz = fromIntegral $ natVal (Proxy @n)

allTransforms :: Int -> (Coord -> a) -> [Coord -> a]
allTransforms len f0 = do
  let flipVert f (r, c) = f (len -1 - r, c)
      rotateCwQt f (r, c) = f (len -1 - c, r)
  f1 <- [f0, flipVert f0]
  take 4 (iterate rotateCwQt f1)

instance Solution Day21 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap id . lines <$> getInputS
    mapM_ print xs
    let grid = words "ABC DEF GHI"
        view (r,c) = grid !! r !! c
        allViews = allTransforms 3 view
        ppr v = forM_ [0..2] \r ->
          putStrLn (fmap (\c -> v( r,c)) [0..2])
    forM_ allViews \v -> do
      ppr v
      putStrLn ""
