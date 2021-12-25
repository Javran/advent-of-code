{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2020.Day21
  (
  )
where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import qualified Data.Map.Strict as M
import Data.Semigroup
import qualified Data.Set as S
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (many)

data Day21 deriving (Generic)

type AllergenInfo = ([String], [String])

allergenInfoP :: ReadP AllergenInfo
allergenInfoP =
  (,) <$> many (wordP <* char ' ') <*> do
    _ <- string "(contains "
    xs <- wordP `sepBy` string ", "
    _ <- string ")"
    pure xs
  where
    wordP = munch1 isAlphaNum

type Assignments = M.Map String String

solve :: M.Map String (S.Set String) -> Assignments -> [Assignments]
solve possibleAllergens assns
  | M.null possibleAllergens = [assns]
  | otherwise = do
    let Just (Min (Arg _ (l, rs))) =
          minimum $ fmap (\p@(_k, v) -> Just (Min (Arg (S.size v) p))) $ M.toList possibleAllergens
    r <- S.toList rs
    let possibleAllergens' = M.map (S.delete r) $ M.delete l possibleAllergens
    guard $ not (any null possibleAllergens')
    solve possibleAllergens' (M.insert l r assns)

instance Solution Day21 where
  solutionRun _ SolutionContext {getInputS, answerS, answerShow} = do
    algInfo <- fmap (fromJust . consumeAllWithReadP allergenInfoP) . lines <$> getInputS
    {-
      if x is definitely contained in list ys ... one of ys must be assigned x.
      we should be able to narrow down search space just by intersection.
     -}
    let possibleAllergens :: M.Map String (S.Set String)
        possibleAllergens = M.fromListWith S.intersection do
          (xs, ys) <- algInfo
          let xSet = S.fromList xs
          y <- ys
          pure (y, xSet)
        can'tBeAllergens =
          S.difference (S.unions $ fmap (S.fromList . fst) algInfo) (S.unions $ M.elems possibleAllergens)
    answerShow $ countLength (`S.member` can'tBeAllergens) $ concatMap fst algInfo
    let assns = head $ solve possibleAllergens M.empty
    answerS $ intercalate "," $ fmap snd $ M.toAscList assns
