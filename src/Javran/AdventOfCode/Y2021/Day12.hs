{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2021.Day12
  (
  )
where

import Control.Monad
import Data.Char
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude

data Day12 deriving (Generic)

isLarge :: String -> Bool
isLarge = any isUpper

type Node = String

type Graph = M.Map Node (S.Set Node)

type Path = [Node]

findPaths :: Graph -> Node -> S.Set Node -> Bool -> [Node] -> [Path]
findPaths graph cur visitedSmalls twiceBudget path
  | cur == "end" = pure (reverse $ cur : path)
  | otherwise = do
    Just nextsSet <- pure $ graph M.!? cur
    next <- S.toList nextsSet
    guard $ next /= "start"
    let nextAlreadyVisited = S.member next visitedSmalls
    guard $
      isLarge next
        || (-- next is a small cave
            not nextAlreadyVisited || twiceBudget)
    let visitedSmalls' =
          if isLarge cur
            then visitedSmalls
            else S.insert cur visitedSmalls
        twiceBudget'
          | isLarge next =
            -- not using it.
            twiceBudget
          | nextAlreadyVisited =
            -- using budget on this one
            False
          | otherwise =
            -- carry over.
            twiceBudget
    findPaths
      graph
      next
      visitedSmalls'
      twiceBudget'
      (cur : path)

instance Solution Day12 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap ((\[a, b] -> (a, b)) . splitOn "-") . lines <$> getInputS
    let graph :: Graph
        graph = M.fromListWith (<>) do
          (u, v) <- xs
          [(u, S.singleton v), (v, S.singleton u)]
    forM_ [False, True] $ \twiceBudget -> do
      answerShow $ length $ findPaths graph "start" (S.singleton "start") twiceBudget []
