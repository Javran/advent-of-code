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

_pprWorld :: World -> String
_pprWorld w = case IS.minView w of
  Nothing -> ""
  Just (minLoc, _) ->
    let maxLoc = IS.findMax w
     in do
          l <- [minLoc .. maxLoc]
          pure $ bool '.' '#' (IS.member l w)

type Rules = M.Map [Bool] Bool

verifyRules :: Rules -> Either String Rules
verifyRules rules = do
  let enforce rawRule = do
        let (lhs, expectedRhs) = consumeOrDie ruleP rawRule
            actualRhs = Just True == rules M.!? lhs
        when (expectedRhs /= actualRhs) do
          Left $ "Unexpected rule for " <> show lhs
  enforce "..... => ."
  enforce "##### => ."
  enforce "....# => ."
  enforce "#.... => ."
  pure rules

{-
  For part 2, obviously simulation won't cut it,
  so I suspected there was an attractor.

  And indeed there is, with a catch: eventually
  the relative location between points are fixed,
  but the pattern as a whole moves in one direction indefinitely.
  Therefore in fact we not only have an attractor, but also
  it is simply a fixpoint with a constant "drift".

  In order to find this fixpoint, we just need to change our view
  of the World, literally:

  we change the representation from World to RebasedWorld = (offset, World),
  so that the resulting world's minimum location is always 0.

  This allows us to focus on the resulting World and see that indeed relative
  locations won't change beyond fixpoint generation.
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
          -}

          let locView = fmap (`IS.member` w) [loc -2 .. loc + 2]
          loc <$ guard (Just True == (rules M.!? locView))

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
  Just (minLoc, _) -> (minLoc, IS.mapMonotonic (subtract minLoc) w)

rWorldSum :: RebasedWorld -> Int
rWorldSum (offset, w) = offset * IS.size w + sum (IS.toList w)

instance Solution Day12 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    [[initStRaw], rulesRaw] <- splitOn [""] . lines <$> getInputS
    let parsedSt = consumeOrDie initStateP initStRaw
        initSt =
          IS.fromDistinctAscList $
            catMaybes $ zipWith (\v i -> if v then Just i else Nothing) parsedSt [0 ..]
        rulesIn =
          M.fromListWith (error "rule conflict") $
            fmap (consumeOrDie ruleP) rulesRaw
        rules = case verifyRules rulesIn of
          Right v -> v
          Left msg -> error $ "Failed to verify rule: " <> msg
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
