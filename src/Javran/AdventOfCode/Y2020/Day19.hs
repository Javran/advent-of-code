{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2020.Day19
  (
  )
where

import Control.Applicative
import Control.Monad
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (many)

data Day19

{-
  Note that it is intentional that this Rule definition
  supports only a limited form of valid syntax - as the problem
  specifically states that the goal of this puzzle is not to go beyond
  what we get as input.
 -}
data Rule
  = -- | matches a string
    RStr String
  | -- | [[a,b], [c,d]] means (a then b) or (c then d)
    RAlt [[Int]]
  deriving (Show)

ruleP :: ReadP Rule
ruleP =
  (RStr <$> tok (char '"' *> munch1 (/= '"') <* char '"'))
    <++ (RAlt
           <$> (many (tok decimal1P)
                  `sepBy` tok (char '|')))
  where
    tok p = p <* skipSpaces

type Rules = IM.IntMap Rule

{-
  performs match, returns unconsumed parts.
 -}
performMatch :: Rules -> [Char] -> Int -> Maybe [Char]
performMatch rules = fix $ \recur xs i -> case rules IM.!? i of
  Nothing -> Nothing
  Just r -> case r of
    RStr ys -> do
      let l = length ys
          (xs0, xs1) = splitAt l xs
      guard $ xs0 == ys
      pure xs1
    RAlt ys ->
      foldl1 (<|>) (fmap (foldM recur xs) ys)

{-
  we could use S.cartesianProduct here, but probably best to not to:
  as the resulting elements would be (a,b) that we have to map back to a single one,
  which rebuilds the whole Set.
 -}
setProd :: (Ord a, Monoid a) => S.Set a -> S.Set a -> S.Set a
setProd sa sb = S.fromList $ do
  a <- S.toList sa
  b <- S.toList sb
  pure (a <> b)

{-
  For acyclic set of rules, we can compute the complete set of strings
  that matches the rule.
 -}
getMatchingSetOf :: Rules -> Int -> S.Set String
getMatchingSetOf rules = memoFix $ \query i ->
  case IM.lookup i rules of
    Nothing -> S.empty
    Just rule -> case rule of
      RStr s -> S.singleton s
      RAlt ys ->
        S.unions
          (fmap (foldl setProd (S.singleton "") . fmap query) ys)

instance Solution Day19 where
  solutionIndex _ = (2020, 19)
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    [rawRules, messages] <- splitOn [""] . lines <$> getInputS
    let rulePairP = (,) <$> (decimal1P <* string ": ") <*> ruleP
        rules :: Rules
        rules =
          IM.fromList $
            fmap (fromJust . consumeAllWithReadP rulePairP) rawRules
    answerShow (countLength (\msg -> Just "" == performMatch rules msg 0) messages)
    let getMatchingSet :: Int -> S.Set String
        getMatchingSet = getMatchingSetOf rules

    {-
      My key observation:

      Notice the following statements are true for both example input and my specific input:

      - We always have `0: 8 11` as the root rule.
        In addition, this seems to be the only use site of rule 8 and rule 11.
      - `8: 42` and `11: 42 31` are present.
      - all matching strings of rule 8 are of the same length l8
      - all matching strings of rule 11 are of the same length l11

      Now, by changing:

      - `8: 42` to `8: 42 | 42 8`
      - `11: 42 31` to `11: 42 31 | 42 11 31`

      we effectively looking for strings that matches the following description:

      > a sequence of {rule 42} repeating m times,
      > followed by a sequence of {rule 31} repeating n times,
      > where m > n > 0.

      Now, if we further assume that there is no string that matches both
      rule 8 and rule 11
      (if l8 and l11 are different, only match string as prefix on the longer set),
      we should be able to separate out substring matching rule 42 or rule 31
      without ambiguity. Then we can verify about m > n > 0, which should identify all matching strings.

     -}

    -- TODO: cleanup
    -- rule 42 matches a string of length 8 exactly.
    -- rule 31 matches a string of length 8 exactly.
    {-
      TODO: we happen to have l42 being exactly the same as l31, which isn't the case for example,
      let's try generalizing this a little bit to handle example as well..
      (we also rely on the assumption that set42 and set31 have no intersection)
     -}
    let set42 = getMatchingSet 42
        set31 = getMatchingSet 31
        isActuallyMatching zs = l42 > 0 && and (zipWith (==) zs1 [31, 31 ..]) && l42 > l31 && l31 > 0
          where
            l42 = length zs0
            l31 = length zs1
            (zs0, zs1) = span (== 42) zs

        messages1 = mapMaybe (mapM matchingChunk . chunksOf 8) $ filter ((== 0) . (`mod` 8) . length) messages
        matchingChunk ts
          | ts `S.member` set42 = Just 42
          | ts `S.member` set31 = Just 31
          | otherwise = Nothing
    answerShow $ countLength isActuallyMatching messages1
