{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2020.Day10
  (
  )
where

{- HLINT ignore "Unused LANGUAGE pragma" -}

import Control.Monad
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import qualified Data.List.Split as LSplit
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP

data Day10

instance Solution Day10 where
  solutionIndex _ = (2020, 10)
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (read @Int) . lines <$> getInputS
    let builtInJolt = maximum xs + 3
        ys = sort (0 : builtInJolt : xs)
        distrib = M.fromListWith (+) $ do
          d <- zipWith (-) (tail ys) ys
          pure (d, 1 :: Int)
    answerShow (distrib M.! 3 * distrib M.! 1)
    let adapters = IS.fromList ys
        countWays = memoFix $ \query i ->
          if
              | i == 0 -> 1 :: Integer
              | IS.member i adapters -> sum $ do
                d <- [1 .. 3]
                let i' = i - d
                guard $ i' >= 0
                pure (query i')
              | otherwise -> 0
    answerShow (countWays builtInJolt)
