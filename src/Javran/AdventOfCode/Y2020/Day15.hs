{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2020.Day15
  (
  )
where

{- HLINT ignore "Unused LANGUAGE pragma" -}

import Control.Monad
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
import Data.Tuple

data Day15 -- TODO: change to actual number

-- TODO: import to Javran.AdventOfCode.Y2020.Main

instance Solution Day15 where
  solutionIndex _ = (2020, 15)
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    [raw] <- lines <$> getInputS
    let xs :: [(Int, Int)]
        xs = zip [1..] $ fmap read $ LSplit.splitOn "," raw
        nextNum lastPair@(turn, num) hist = case hist IM.!? num of
          Nothing -> 0
          Just turn' -> turn - turn'
        ys = unfoldr go (last xs, IM.fromList (fmap swap (init xs)))
          where
            go (lastPair@(turn, num), hist) = m `seq` Just ((turn+1, x), ((turn+1, x), m))
              where
                m =  IM.insert num turn hist
                x = nextNum lastPair hist
        ns = xs <> ys
    answerShow (snd $ ns !! (2020-1))
    answerShow (snd $ ns !! (30000000-1))
