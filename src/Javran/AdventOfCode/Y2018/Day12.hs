{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2018.Day12
  (
  )
where

import Control.Monad
import Data.Function
import qualified Data.IntSet as IS
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day12 deriving (Generic)

boolP :: ReadP Bool
boolP = (False <$ char '.') <++ (True <$ char '#')

initStateP :: ReadP [Bool]
initStateP = string "initial state: " *> many1 boolP

ruleP :: ReadP ([Bool], Bool)
ruleP = do
  lhs <- many1 boolP
  _ <- string " => "
  rhs <- boolP
  pure (lhs, rhs)

_pprBools :: [Bool] -> String
_pprBools = fmap (bool '.' '#')

_pprWorld :: World -> String
_pprWorld w = case IS.minView w of
  Nothing -> ""
  Just (minLoc, _) ->
    let maxLoc = IS.findMax w
     in do
          l <- [minLoc .. maxLoc]
          pure $ bool '.' '#' (IS.member l w)

type Rules = M.Map [Bool] Bool

{-
  TODO: for part 2, obviously simulation won't cut it,
  so I suspect there's an attractor.

  First step, let's use a sparse rep of the state,
  which should make the simulation scale a bit,
  then we can try to determine if there's an attractor.

  Update: it doesn't appear to be an attractor,
  as the min and max of World expands indefinitely.

  However, I notice for both example input and login input,
  the # of active elements in the World eventually goes
  to a fix amount and never change,
  so we can probably utilize this property somehow.

 -}

type World = IS.IntSet

step :: Rules -> World -> World
step rules w = case IS.minView w of
  Nothing -> w
  Just (minLoc, _) ->
    let maxLoc = IS.findMax w
     in IS.fromDistinctAscList do
          loc <- [minLoc -1 .. maxLoc + 1]
          {-
            we technically need to examine `minLoc - 2` and `maxLoc + 2`,
            in case the following is part of the rule:

            ....# => #
            #.... => #

            however, for both example and my login, we have the following
            as part of the rule:

            ....# => .
            #.... => .

            meaning we can cut padding to just 3.

            TODO: verify this assumption about the rule.

          -}

          let locView = fmap (`IS.member` w) [loc -2 .. loc + 2]
              after = Just True == (rules M.!? locView)
          guard after
          pure loc

{-
  A rebased world always have the minimal location being 0,
  the offset is tracked as a seperated Int.
 -}
type RebasedWorld = (Int, World)

{-
  Normalizes the minimal location so that it is always 0.
 -}
rebase :: World -> RebasedWorld
rebase w = case IS.minView w of
  Nothing -> (0, w)
  Just (minLoc, _) -> (minLoc, IS.map (subtract minLoc) w)

rWorldSum :: RebasedWorld -> Int
rWorldSum (offset, w) = offset * IS.size w + sum (IS.toList w)

instance Solution Day12 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    [[initStRaw], rulesRaw] <- splitOn [""] . lines <$> getInputS
    let parsedSt = consumeOrDie initStateP initStRaw
        initSt =
          IS.fromDistinctAscList $
            catMaybes $ zipWith (\v i -> if v then Just i else Nothing) parsedSt [0 ..]
        rules =
          M.fromListWith (error "rule conflict") $
            fmap (consumeOrDie ruleP) rulesRaw
        progression :: [(Int, RebasedWorld)]
        progression = fmap (second rebase) $ zip [0 ..] $ iterate (step rules) initSt

    do
      let (_, w) = progression !! 20
      answerShow $ rWorldSum w
    do
      let ((startGen, (minLocBefore, w)), (_, (minLocAfter, _))) : _ =
            dropWhile (uncurry ((/=) `on` (snd . snd))) $ zip progression (tail progression)
          incr = minLocAfter - minLocBefore
          slowSolve targetGen =
            -- we should get the exact same result as fastSolve beyond fixpoint.
            rWorldSum $ snd $ progression !! targetGen
          fastSolve targetGen = rWorldSum (minLocBefore + incr * (targetGen - startGen), w)
          verify = False
      {-
        Starting from generation startGen,
        we have a fixpoint that "drifts" by (minLocAfter - minLocBefore) everytime.
       -}
      when verify $
        print (all (\n -> fastSolve n == slowSolve n) (take 20 [startGen ..]))
      answerShow $ fastSolve 50000000000
