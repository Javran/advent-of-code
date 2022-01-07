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

module Javran.AdventOfCode.Y2018.Day17
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
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

data Day17 deriving (Generic)

type Coord = (Int, Int) -- x, y

inputLineP :: ReadP [Coord]
inputLineP = do
  (cNext, mk) <-
    (( 'y'
     , \x ys -> [(x, y) | y <- ys]
     )
       <$ char 'x')
      <++ (( 'x'
           , (\y xs -> [(x, y) | x <- xs])
           )
             <$ char 'y')
  _ <- char '='
  u <- decimal1P
  _ <- string (", " <> [cNext] <> "=")
  vFrom <- decimal1P
  _ <- string ".."
  vTo <- decimal1P
  pure (mk u [vFrom .. vTo])

{-
  TODO: I suspect we are looking at some simulation again:

  - keep a list of x of springs (resulting from branching the very first one),
    and y-coord that we are currently scanning.
  - moving down until we "hit" something, set `|` mark appropriately.
  - now we have few situations to deal with

    + if we cannot find bound on either side, find locations that we can
      branch this spring and keep going down, set `|` mark appropriately.

    + if we find bound one side, set `|` mark appropriately,
      and "redirect" current spring, as we are leaking from only one side.

    + if we find bound both sides, set `~` as appropriate, then
      we have to move y back up instead of down to "fill the reservoir"

 -}

instance Solution Day17 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (consumeOrDie inputLineP) . lines <$> getInputS
    let coords = S.fromList (concat xs)
        Just (MinMax2D ((minX, maxX), (minY, maxY))) = foldMap (Just . minMax2D) (S.toList coords)
    forM_ [minY .. maxY] \y -> do
      let render x =
            if S.member (x, y) coords
              then '█'
              else ' '
      putStrLn $ "|" <> (fmap render [minX .. maxX]) <> "|"
