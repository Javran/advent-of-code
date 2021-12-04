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

data Rule
  = RStr String
  | RSeq [Int]
  | RAlt [Rule]
  deriving (Show)

ruleP :: ReadP Rule
ruleP =
  (RStr <$> tok (char '"' *> munch1 (/= '"') <* char '"'))
    <++ (RAlt
           <$> ((RSeq <$> many (tok decimal1P))
                  `sepBy` tok (char '|')))
  where
    tok p = p <* skipSpaces

performMatch rules xs i = case rules IM.!? i of
  Nothing -> Nothing
  Just r -> case r of
    RStr ys -> do
      let l = length ys
          (xs0, xs1) = splitAt l xs
      guard $ xs0 == ys
      pure xs1
    RSeq ys -> processRSeq ys
    RAlt ys -> foldl1 (<|>) (fmap (\(RSeq zs) -> processRSeq zs) ys)
  where
    processRSeq ys = foldM (\curXs j -> performMatch rules curXs j) xs ys

instance Solution Day19 where
  solutionIndex _ = (2020, 19)
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- splitOn [""] . lines <$> getInputS
    let [rawRules, messages] = xs
        setProd sa sb = S.fromList $ do
          {-
            we could use S.cartesianProduct here, but probably best to not to:
            as the resulting elements would be (a,b) that we have to map back to a single one,
            which rebuilds the whole Set.
           -}
          a <- S.toList sa
          b <- S.toList sb
          pure (a <> b)
        rulePairP =
          (,)
            <$> (decimal1P <* string ": ")
            <*> ruleP
        rules :: IM.IntMap Rule
        rules =
          IM.fromList $
            fmap
              (fromJust
                 . consumeAllWithReadP rulePairP)
              rawRules
        getMatchingSet :: Int -> S.Set String
        getMatchingSet = memoFix $ \query i ->
          let processRSeq ys = foldl setProd (S.singleton "") $ fmap query ys
           in case IM.lookup i rules of
                Nothing -> S.empty
                Just rule -> case rule of
                  RStr s -> S.singleton s
                  RSeq ys -> processRSeq ys
                  RAlt ys -> S.unions (fmap (\(RSeq zs) -> processRSeq zs) ys)
    answerShow (countLength (\msg -> Just "" == performMatch rules msg 0) messages)
    -- TODO: cleanup
    -- note: `0: 8 11' seems to be the only case that uses 8 and 11.
    -- 8 repeats 42 at least once
    -- 11 does 42 31, 42 42 31 31 ... etc. where there is same amount of 42 and 31
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
