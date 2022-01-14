{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2017.Day17
  (
  )
where

import Control.Monad
import Control.Monad.ST
import Data.List
import Data.STRef
import Javran.AdventOfCode.Prelude

data Day17 deriving (Generic)

data Node s = Node
  { nVal :: Int
  , nNext :: STRef s (Node s)
  }

type NodeRef s = STRef s (Node s)

forward :: NodeRef s -> ST s (NodeRef s)
forward n = nNext <$> readSTRef n

buildBuffer :: Int -> Int -> ST s (NodeRef s, NodeRef s)
buildBuffer skipLen tillN = do
  sInit <- mdo
    s <- newSTRef $ Node 0 s
    pure s
  let forwardN n =
        if skipLen < 0
          then error "negative forward count"
          else (foldl' (>=>) pure $ replicate skipLen forward) n
      oneStep n0 val = do
        n1 <- forwardN n0
        Node _ n1Next <- readSTRef n1
        n1Next' <- newSTRef $ Node val n1Next
        modifySTRef' n1 \n -> n {nNext = n1Next'}
        pure n1Next'
  sFinal <- foldM (\cur val -> oneStep cur val) sInit [1 .. tillN]
  pure (sInit, sFinal)

type State2 = (Int, Maybe Int)

{-
  Note that when inserting value `val`, the list before insertion
  also has length `val`.
 -}
simulate :: Int -> Int -> State2 -> State2
simulate skipLen val (loc0, acc) =
  -- important that we don't pile this up.
  acc' `seq` (loc1, acc')
  where
    {- `rem` puts output in range [0 .. val-1],
       meaning loc1 is actually always in range.
     -}
    loc1 = (loc0 + skipLen) `rem` val + 1
    acc' = if loc1 == 1 then Just val else acc

instance Solution Day17 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    n <- read @Int . head . lines <$> getInputS
    answerShow $ runST do
      (_, sFinal) <- buildBuffer n 2017
      nVal <$> (readSTRef =<< forward sFinal)
    let (_, Just ans) =
          foldl'
            (\st val -> simulate n val st)
            (0, Nothing)
            [1 .. 50_000_000]
    answerShow ans
