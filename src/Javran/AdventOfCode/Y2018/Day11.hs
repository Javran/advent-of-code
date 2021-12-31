{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2018.Day11
  (
  )
where

import Control.Monad
import qualified Data.Array.ST as Arr
import qualified Data.Array.Unboxed as UArr
import Data.List
import Javran.AdventOfCode.Prelude

data Day11 deriving (Generic)

type Coord = (Int, Int) -- X and Y

powerLevel :: Int -> Coord -> Int
powerLevel serial (x, y) = p3 - 5
  where
    rackId = x + 10
    p0 = rackId * y
    p1 = p0 + serial
    p2 = p1 * rackId
    p3 = (p2 `quot` 100) `rem` 10

{-
  See: https://en.wikipedia.org/wiki/Prefix_sum
  Prefix sum allows us to do some preprocessing
  and then get sum of consecutive regions for a 1D array:

  let s[0] = 0, s[i] = sum of a[1], a[2], .. a[i] (a is 1-based array).

  then sum of a[m], a[m+1] .. a[n] is just s[n] - s[m-1].

  For doing part 2 in an efficient manner, we can
  generalize this idea to 2D:

  For a[i,j] where both dimensions are 1-based:

  0 --- + --- + -> x
  |  A  |  B  |
  + --- v --- +
  |  C  |  D  |
  + --- + --- u
  |
  v
  y

  where u is the bottom-right corner of a[x1,y1],
    v the top-left corner of a[x0,y0],

  let s[i,j] be the 2D accumulated sum:

  s[i,j] = 0 (i == 0 or j == 0)
  s[i,j] = sum of a[1,1], a[1,2], ... a[i,j]

  we can compute sum of area (x0,y0) ~ (x1,y1) as:

  sum(a[x0, y0] .. a[x1,y1])
  => D
  => (A+B+C+D) - (A+C) - (A+B) + A
  => s[x1,y1] - s[x0-1,y1] - s[x1,y0-1] + s[x0-1,y0-1]

  And the exact same algebra gives us a way to compute s as well:

  (A+B+C+D) - (A+C) - (A+B) + A = D
  ==> (A+B+C+D) = D + (A+C) + (A+B) - A
  ==> s[x,y] = a[x,y] + s[x-1,y] + s[x,y-1] - s[x-1,y-1]

 -}

computeAccumulated :: (Coord -> Int) -> UArr.UArray (Int, Int) Int
computeAccumulated getVal = Arr.runSTUArray do
  let sz = 300
  arr <- Arr.newArray ((0, 0), (sz, sz)) (0 :: Int)
  forM_ [1 .. sz] \x ->
    forM_ [1 .. sz] \y -> do
      let v = getVal (x, y)
      ~[v0, v1, v2] <- mapM (Arr.readArray arr) [(x -1, y), (x, y -1), (x -1, y -1)]
      Arr.writeArray arr (x, y) $! v + v0 + v1 - v2
  pure arr

instance Solution Day11 where
  solutionRun _ SolutionContext {getInputS, answerS} = do
    n <- read @Int <$> getInputS
    let accd = computeAccumulated (powerLevel n)
        querySum (x0, y0) (x1, y1) =
          accd UArr.! (x1, y1)
            - accd UArr.! (x0 -1, y1)
            - accd UArr.! (x1, y0 -1)
            + accd UArr.! (x0 -1, y0 -1)

    do
      let ((ansX, ansY), _) = maximumBy (comparing snd) do
            x <- [1 .. 300 - 3 + 1]
            y <- [1 .. 300 - 3 + 1]
            let topLeft = (x, y)
                totalPower = querySum topLeft (x + 2, y + 2)
            pure (topLeft, totalPower)
      answerS $ show ansX <> "," <> show ansY
    do
      let (((ansX, ansY), size), _) = maximumBy (comparing snd) do
            x <- [1 .. 300]
            y <- [1 .. 300]
            (sz, bottomRight) <-
              unfoldr
                (\sz -> do
                   let br@(x', y') = (x + sz -1, y + sz -1)
                   guard $ x' <= 300 && y' <= 300
                   pure ((sz, br), sz+1))
                1
            let topLeft = (x, y)
                totalPower = querySum topLeft bottomRight
            pure ((topLeft, sz), totalPower)
      answerS $ show ansX <> "," <> show ansY <> "," <> show size
