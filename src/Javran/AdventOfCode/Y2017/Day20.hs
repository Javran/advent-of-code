{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2017.Day20
  (
  )
where

import Control.Monad
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import qualified Data.Map.Strict as M
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.TestExtra
import Linear.Affine
import Linear.V3
import Linear.Vector
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day20 deriving (Generic)

type Loc = Point V3 Int

type Pt = (Loc, V3 Int, V3 Int)

ptP :: ReadP Pt
ptP =
  (,,)
    <$> (char 'p' *> (P <$> vecP) <* string ", ")
    <*> (char 'v' *> vecP <* string ", ")
    <*> (char 'a' *> vecP)
  where
    intP = readS_to_P (reads @Int)
    vecP = between (string "=<") (char '>') do
      [a, b, c] <- intP `sepBy1` char ','
      pure $ V3 a b c

{-
  t=0: a, v, p
  t=1: a, v + a, p + (v + a)
  t=2: a, v + 2a, p + (v + a) + (v + 2a)
  ...
 -}
locAtTime :: Pt -> Int -> Loc
locAtTime (p, v, a) t = p .+^ (v ^* t) .+^ (a ^* ((t + 1) * t `quot` 2))

type System = (IM.IntMap Pt, Int)

step :: System -> System
step (m, t) = (IM.withoutKeys m (IS.fromList collisions), t')
  where
    t' = t + 1
    locs = M.fromListWith (<>) do
      (i, pt) <- IM.toList m
      pure (locAtTime pt t', [i])
    collisions =
      concatMap
        (\(_k, vs) -> case vs of
           [] -> unreachable
           [_] -> []
           _ -> vs)
        $ M.toList locs

allEqual :: Eq a => [a] -> Bool
allEqual = \case
  [] -> True
  x : xs -> all (== x) xs

{-
  Takes as input a predicate `isStable` and an infinite list of observations over time,
  dropping leading observations until it is considered stable.

  It is expected that `isStable` works on infinite list
  (which is to allow the predicate to take a sliding window of arbitrary len as it wishes)
 -}
dropUntilStable :: ([a] -> Bool) -> [a] -> [(Int, a)]
dropUntilStable isStable = go . zip [0 ..]
  where
    go xs@(~(_ : tl)) =
      if isStable (fmap snd xs)
        then xs
        else go tl

{-
  The gist of my solution to both part 1 and part 2 is just simulation.
  The correctness is not guaranteed, but it looks good enough.

  For part 1, after working out an algebratic way of computing particle locations
  at any time, we can fast forward the system efficiently,
  and stop when the point closest to origin,
  no longer changes over a sliding window (the system is considered stable at this point).
  (here the magic value chosen is 100 for step length, 10 for sliding window)

  For part 2, we just simulate the system - for this problem, particle locations
  are only defined over discrete time units, meaning we don't have to think about
  issues like "what if two particles collide at t = 1.5", or any non-integer value of t.
  Now to solve this part, we just do the exact same thing we did to part 1: simulate until
  the system stablizes over a sliding window.

  It probably doesn't matter much, but the guess can be slight educated:
  notice in part 1, we've established a timestamp (I call it `guessedStableIter`),
  beyond which the system is considered stable.
  It should then be a fair assumption that we don't actually need to examine
  the system prior to this timestamp for part 2 - by skipping over the "unstable"
  part of time, we can avoid running into some false stable points.

 -}

instance Solution Day20 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (extraOps, rawInput) <- consumeExtra getInputS
    let xs = fmap (consumeOrDie ptP) . lines $ rawInput
        (runPart1, runPart2) = shouldRun extraOps
    let tracedXs :: [(Int, Pt)]
        tracedXs = zip [0 :: Int ..] xs
    guessedStableIter <-
      if runPart1
        then do
          let stepLen =
                {-
                  We can afford to fast forward as computing location
                  at specific times is cheap.
                 -}
                100
              observations =
                fmap
                  (\t ->
                     -- observe current candidate
                     fst . minimumBy (comparing snd) $
                       -- computes the system at time t.
                       fmap (second \x -> manhattan 0 $ locAtTime x t) tracedXs)
                  [0, stepLen ..]
              isStable = allEqual . take 10
              (stable', ans) : _ = dropUntilStable isStable observations
          answerShow ans
          pure (stable' * stepLen)
        else pure 50
    when runPart2 do
      let observations =
            -- observe number of particles.
            fmap (IM.size . fst) $
              -- step the system
              iterate step (IM.fromList tracedXs, 0)
          isStable = allEqual . take 10
          (_, ans) : _ =
            dropUntilStable isStable $
              drop guessedStableIter observations
      answerShow ans
