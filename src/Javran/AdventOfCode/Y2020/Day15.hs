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
import Data.Word
import Javran.AdventOfCode.Prelude

data Day15

solve :: [(Word32, Word32)] -> Int -> Word32
solve xs size = runST $ do
  vec <- VUM.replicate size (0 :: Word32)
  forM_ (init xs) $ \(t, v) ->
    VUM.unsafeWrite vec (fromIntegral v) t
  let (startT, startN) = last xs
  fix
    (\loop turn val ->
       if turn == fromIntegral size
         then pure val
         else do
           let val' = fromIntegral val
           r <- VUM.unsafeRead vec val'
           let num' = if r == 0 then 0 else turn - r
               turn' = turn + 1
           VUM.unsafeWrite vec val' turn
           num' `seq` turn' `seq` loop turn' num')
    startT
    startN

instance Solution Day15 where
  solutionIndex _ = (2020, 15)
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (extraOpts, rawInput) <- consumeExtraLeadingLines <$> getInputS
    let [raw] = lines rawInput
    let xs :: [(Word32, Word32)]
        xs = zip [1 ..] $ fmap read $ splitOn "," raw
    answerShow $ solve xs 2020
    let n = case extraOpts of
          Nothing -> 30000000
          Just [overrideN] -> read overrideN
          _ -> errInvalid
    answerShow $ solve xs n
