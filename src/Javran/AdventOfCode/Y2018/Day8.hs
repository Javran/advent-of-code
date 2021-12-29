{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2018.Day8
  (
  )
where

import Control.Monad
import Control.Monad.State.Strict
import Data.Functor.Base (TreeF (NodeF))
import Data.Functor.Foldable
import Data.Monoid
import Data.Tree
import Javran.AdventOfCode.Prelude

data Day8 deriving (Generic)

type N = Tree [Int]

parseNode :: State [Int] N
parseNode = do
  ~[n, m] <- state (splitAt 2)
  subForest <- replicateM n parseNode
  rootLabel <- state (splitAt m)
  pure Node {rootLabel, subForest}

metaSum :: N -> Int
metaSum = cata \case
  NodeF meta rs -> sum meta + sum rs

nodeValue :: N -> Int
nodeValue = cata \case
  NodeF meta rs -> case rs of
    [] -> sum meta
    _ : _ ->
      let sz = length rs
       in getSum $
            foldMap
              (\i ->
                 if i > 0 && i <= sz
                   then Sum $ rs !! (i -1)
                   else 0)
              meta

instance Solution Day8 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (read @Int) . words . head . lines <$> getInputS
    let n = evalState parseNode xs
    answerShow (metaSum n)
    answerShow (nodeValue n)
