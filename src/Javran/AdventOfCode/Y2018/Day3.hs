{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2018.Day3
  (
  )
where


import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day3 deriving (Generic)

type Claim = (Int, ((Int, Int), (Int, Int)))

claimP :: ReadP Claim
claimP = do
  cId <- char '#' *> decimal1P
  _ <- string " @ "
  xy <- (,) <$> decimal1P <*> (char ',' *> decimal1P)
  _ <- string ": "
  wt <- (,) <$> decimal1P <*> (char 'x' *> decimal1P)
  pure (cId, (xy, wt))

instance Solution Day3 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (consumeOrDie claimP) . lines <$> getInputS
    let m = M.fromListWith (<>) do
          (cId, ((x, y), (w, t))) <- xs
          x' <- [x .. x + w -1]
          y' <- [y .. y + t -1]
          pure ((x', y'), [cId])
        overlaps =
          M.filter
            (\case
               _ : _ : _ -> True
               _ -> False)
            m
        conflicts = IS.unions $ fmap IS.fromList $ M.elems overlaps
    answerShow $ M.size overlaps
    answerShow $ head $ filter (`IS.notMember` conflicts) (fmap fst xs)
