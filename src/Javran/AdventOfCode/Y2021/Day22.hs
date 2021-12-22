{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
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

module Javran.AdventOfCode.Y2021.Day22
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Bifunctor
import Data.Bool
import Data.Char
import Data.Coerce
import Data.Either
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashTable.ST.Cuckoo as HT
import Data.Hashable
import Data.Int
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import qualified Data.List.Ordered as LOrd
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Debug.Trace
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Linear.Affine
import Linear.V3
import Text.ParserCombinators.ReadP hiding (count, many)

data Day22 deriving (Generic)

type Seg = (MinMax Int, MinMax Int, MinMax Int)

segP :: ReadP (Bool, Seg)
segP = do
  let intP = readS_to_P (reads @Int)
      rangeP = do
        _ <- satisfy (`elem` "xyz")
        (,) <$> (char '=' *> intP <* string "..") <*> intP
  o <- (True <$ string "on ") <++ (False <$ string "off ")
  [xr, yr, zr] <- rangeP `sepBy1` char ','
  pure $ coerce (o, (xr, yr, zr))

volume :: Seg -> Int
volume (x, y, z) = len x * len y * len z
  where
    len (MinMax (l, r)) = r - l + 1

intersectSeg :: Seg -> Seg -> Maybe Seg
intersectSeg (a, b, c) (d, e, f) = do
  [x, y, z] <- sequence (zipWith intersectMinMax [a, b, c] [d, e, f])
  pure $ (x, y, z)

intersectMinMax :: MinMax Int -> MinMax Int -> Maybe (MinMax Int)
intersectMinMax (MinMax (a, b)) (MinMax (c, d)) = do
  let r = min b d
      l = max a c
      delta = r - l + 1
  MinMax (l, r) <$ guard (delta > 0)

solve :: [(Bool, Seg)] -> Int
solve xs = sum vols
  where
    vols =
      unfoldr
        (\(rs, todos) -> do
           ((v, seg) : todos') <- pure todos
           pure
             ( if v then volume seg - overlap seg rs else 0
             , (seg : rs, todos')
             ))
        ([], reverse xs)
    overlap :: Seg -> [Seg] -> Int
    overlap seg segs =
      sum $
        mapMaybe
          (\ss -> do
             (curSeg : segs') <- pure ss
             interSeg <- intersectSeg seg curSeg
             pure $ volume interSeg - overlap interSeg segs')
          $ tails segs

instance Solution Day22 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (consumeOrDie segP) . lines <$> getInputS
    do
      let smallSeg = let s = MinMax (-50, 50) in (s, s, s)
          xs' =
            mapMaybe
              (\(v, s) -> do
                 s' <- intersectSeg smallSeg s
                 pure (v, s'))
              xs
      answerShow (solve xs')
    do
      answerShow (solve xs)
