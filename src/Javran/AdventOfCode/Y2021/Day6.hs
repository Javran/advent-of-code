{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2021.Day6
  (
  )
where

import qualified Data.IntMap.Strict as IM
import Data.List.Split hiding (sepBy)
import Javran.AdventOfCode.Prelude

data Day6 deriving (Generic)

type FishState = IM.IntMap Int -- key: time before reproduce, value: how many

dense :: [Int] -> FishState
dense = IM.fromListWith (+) . fmap (,1)

step :: FishState -> FishState
step m = case m IM.!? 0 of
  Nothing -> tickDown m
  Just cnt ->
    IM.unionWith
      (+)
      (IM.fromList [(6, cnt), (8, cnt)])
      $ tickDown $ IM.delete 0 m
  where
    tickDown = IM.mapKeysMonotonic pred

{-
  Side note: we could speed up transformation by using square matrix:

  [ 0 0 0 0 0 0 0 0 1
  ; 1 0 0 0 0 0 0 0 0
  ; 0 1 0 0 0 0 0 0 1
  ; 0 0 1 0 0 0 0 0 0
  ; 0 0 0 1 0 0 0 0 0
  ; 0 0 0 0 1 0 0 0 0
  ; 0 0 0 0 0 1 0 0 0
  ; 0 0 0 0 0 0 1 0 0
  ; 0 0 0 0 0 0 0 1 0
  ]

  raise it to 256 power, and we can get a result by right-mult initial state to it.

 -}
instance Solution Day6 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    [xs] <- lines <$> getInputS
    let initFs = fmap (read @Int) $ splitOn "," xs
        history = iterate step (dense initFs)
    answerShow $ sum (history !! 80)
    answerShow $ sum (history !! 256)
