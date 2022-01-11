{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2017.Day5
  (
  )
where

import Control.Monad.ST
import Data.Function
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Javran.AdventOfCode.Prelude

data Day5 deriving (Generic)

simulate :: (Int -> Int -> Int) -> [Int] -> ST s Int
simulate modifier xs = do
  vec <- VU.unsafeThaw (VU.fromList xs)
  fix
    (\loop cur cnt -> do
       offset <- VUM.unsafeRead vec cur
       let next = cur + offset
       if next < 0 || next >= VUM.length vec
         then pure (cnt + 1)
         else do
           VUM.unsafeModify vec (modifier offset) cur
           loop next (cnt + 1))
    0
    0

instance Solution Day5 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (read @Int) . lines <$> getInputS
    answerShow $ runST (simulate (const succ) xs)
    answerShow $ runST (simulate (\offset -> if offset >= 3 then pred else succ) xs)
