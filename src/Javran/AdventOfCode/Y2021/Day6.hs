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
import GHC.Generics (Generic)
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

instance Solution Day6 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    [xs] <- lines <$> getInputS
    let initFs = fmap (read @Int) $ splitOn "," xs
        history = iterate step (dense initFs)
    answerShow $ sum (history !! 80)
    answerShow $ sum (history !! 256)
