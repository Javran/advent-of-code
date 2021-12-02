{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Javran.AdventOfCode.Y2020.Day15
  (
  )
where

import Control.Monad
import Control.Monad.ST
import Data.Function
import qualified Data.Vector.Unboxed.Mutable as VUM
import Javran.AdventOfCode.Prelude
import Data.Word

data Day15

solve :: [(Word32, Word32)] -> Int -> Word32
solve xs size = runST $ do
  vec <- VUM.replicate size 0
  forM_ (init xs) $ \(t, v) ->
    VUM.unsafeWrite vec (fromIntegral v) t
  let (startT, startN) = last xs
  fix
    (\loop turn val ->
       if turn == fromIntegral size
         then pure val
         else do
           r <- VUM.unsafeRead vec (fromIntegral val)
           let num' = if r == 0 then 0 else turn - fromIntegral r
               turn' = turn + 1
           VUM.unsafeWrite vec (fromIntegral val) turn
           num' `seq` turn' `seq` loop turn' num')
    startT
    startN

instance Solution Day15 where
  solutionIndex _ = (2020, 15)
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    [raw] <- lines <$> getInputS
    let xs :: [(Word32, Word32)]
        xs = zip [1 ..] $ fmap read $ splitOn "," raw
    answerShow $ solve xs 2020
    answerShow $ solve xs 30000000
