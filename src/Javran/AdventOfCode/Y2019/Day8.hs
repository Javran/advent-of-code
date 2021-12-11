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
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2019.Day8
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Bool
import Data.Char
import Data.Coerce
import Data.Either
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, many)

data Day8 deriving (Generic)

countLayerPixel :: String -> (Int, (Int, Int))
countLayerPixel =
  coerce
    . foldMap
      (\x ->
         ( if x == '0' then 1 :: Sum Int else 0
         , ( if x == '1' then 1 :: Sum Int else 0
           , if x == '2' then 1 :: Sum Int else 0
           )
         ))

{-
  combinePixel u v combines u on top of v.
 -}
combinePixel :: Char -> Char -> Char
combinePixel u v =  if u /= '2' then u else v

instance Solution Day8 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    let (width, height) = (25, 6)
    xs <- chunksOf (width * height) . head . lines <$> getInputS
    let counted = fmap countLayerPixel xs
    let (_, (u, v)) = minimumBy (comparing fst) counted
    answerShow $ u * v
    let tr '1' = '#'
        tr '0' = ' '
    mapM_ (putStrLn . fmap tr)  $ chunksOf width $ foldr1 (zipWith combinePixel) xs
