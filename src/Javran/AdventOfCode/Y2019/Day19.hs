{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Javran.AdventOfCode.Y2019.Day19
  (
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Function
import qualified Data.IntMap.Strict as IM
import Data.Semigroup
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2019.IntCode

{-
  Note: unfortunately, the nature of this puzzle is that we
  are dealing with a blackbox that has some obvious properties
  (like beams are bounded by straightlines) and this solution is specific
  to the input data I received - I'll attempt to write it in a more general
  manner but I won't be surprised if it doesn't work given a different set of
  input IntCode program.

 -}

data Day19 deriving (Generic)

type Coord = (Int, Int)

probes :: [Int] -> [Coord] -> IO [(Coord, Int)]
probes code = mapM \c@(x, y) -> do
  (_, [r]) <- liftIO $ runProgram code [x, y]
  pure (c, r)

instance Solution Day19 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    code <- parseCodeOrDie <$> getInputS
    let sz = 50
        area = [(x, y) | y <- [0 .. sz -1], x <- [0 .. sz -1]]
    results <- probes code area
    answerShow $ countLength (\(_, r) -> r == 1) results

    {-
      Looking at the output of part 1, the following assumptions
      are probably safe:

      - the beam always start at origin.

      - the beam is bounded by two straight lines.
        If not straight lines, they are at least monotonic so that
        binary search can apply.
     -}

    do
      let ysWithMaxX =
            {-
              Looking at part 1 results and make a table from
              y to max x, where (x,y) is being pulled.
             -}
            IM.fromListWith (<>) do
              ((x, y), 1) <- results
              guard $ y >= 10 -- to reduce low-denominator noise
              pure (y, Just (Max x))
          rLo :: Double
          Just (Min rLo) =
            {-
              Assume that there is a fraction r so that we can estimate x given y by:

                x = r * y

              and it's entirely possily that r = p/q in which p and q are large integers
              that we can't reverse-engineer. So the best bet is to keep r as a floatting point.

              Therefore we compute the minimum of r = x / y given ysWithMaxX that
              we've computed in the previous step.

              Some playing around with the bounds suggest that lower bound of x / y
              is reliably giving us estimate of x, where (x,y) is definitely pulling,
              while upper bound seems to be less reliably when y becomes larger.

             -}
            mconcat do
              (y, Just (Max x)) <- IM.toList ysWithMaxX
              let p = fromIntegral x / fromIntegral y
              pure (Just $ Min p)

      let searchForMaxX y = do
            {- finds max x given y, such that (x,y) is pulling. -}
            let xLo = floor (rLo * fromIntegral y)
                xHi = xLo * 2 -- just need some value that is definitely outside.
            fix
              (\continueWith l r -> do
                 {-
                   INVARIANT: l is inside (being pulled) and r is not,
                   we are looking at some point at the boundary but inside.
                  -}
                 let x = (l + r) `quot` 2
                 [(_, v)] <- probes code [(x, y)]
                 case v of
                   1 -> if l /= x then continueWith x r else pure l
                   0 -> if r /= x then continueWith l x else pure l
                   _ -> unreachable)
              xLo
              xHi

      let yLoInit = 49 -- we know this is definitely too small
          fitTest y = do
            when (y == 950) $
              -- see below.
              error "y = 950"
            xMax <- searchForMaxX y
            [(_, v)] <- probes code [(xMax -99, y + 99)]
            pure (v == 1, xMax)

      -- find some y that we can definitely fit a 100x100 in.
      (yLo, yHi) <-
        fix
          (\tryNext lo hi -> do
             (canFit, _) <- fitTest hi
             if canFit
               then pure (lo, hi)
               else tryNext hi (hi * 2))
          yLoInit
          (yLoInit * 2)
      {-
        We are looking at something funny here:
        > mapM (\y -> (\(canFit, _) -> (y, canFit)) <$> fitTest y) [945 .. 955] >>= mapM_ print

        (945,False)
        (946,False)
        (947,False)
        (948,True)
        (949,True)
        (950,False)
        (951,True)
        (952,True)
        (953,True)
        (954,True)
        (955,True)

        In general it looks like monotonic, but it could be flimzy at boundary like this,
        I'm not looking at my specfic input and see how to deal with this problem in general,
        so let's just find lower and upper bound for y by avoiding testing y=950 at all.

       -}
      let search2 l r = do
            {-
              INVARIANT: l can't fit but r can,
              we are looking for one at the boundary that fits.
             -}
            let y = (l + r) `quot` 2
            (canFit, x) <- fitTest y
            if canFit
              then (if r /= y then search2 l y else pure (x, r))
              else
                (if l /= y
                   then search2 y r
                   else do
                     xm <- searchForMaxX r
                     pure (xm, r))
      (xTopRight, yTopRight) <- search2 yLo yHi
      answerShow ((xTopRight - 99) * 10000 + yTopRight)
