{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2018.Day8
  (
  )
where

import Control.Monad
import Control.Monad.State.Strict
import Javran.AdventOfCode.Prelude

data Day8 deriving (Generic)

data Node = Node
  { nChildren :: [Node]
  , nMeta :: [Int]
  }
  deriving (Show)

parseNode :: State [Int] Node
parseNode = do
  xs <- state (splitAt 2)
  let [n, m] = xs
  nChildren <- replicateM n parseNode
  nMeta <- state (splitAt m)
  pure Node {nChildren, nMeta}

metaSum :: Node -> Int
metaSum Node {nChildren, nMeta} = sum (fmap metaSum nChildren) + sum nMeta

nodeValue :: Node -> Int
nodeValue Node {nChildren, nMeta} = case nChildren of
  [] -> sum nMeta
  _ : _ ->
    let sz = length nChildren
        inds = concatMap (\i -> [i-1 | i > 0 && i <= sz]) nMeta
    in sum $ fmap (nodeValue . (nChildren !!)) inds

instance Solution Day8 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (read @Int) . words . head . lines <$> getInputS
    let n = evalState parseNode xs
    answerShow (metaSum n)
    answerShow (nodeValue n)
