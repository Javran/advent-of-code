{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2015.Day9
  (
  )
where

import Control.Monad
import Data.Char
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day9 deriving (Generic)

type Edge = (String, (String, Int))

edgeP :: ReadP Edge
edgeP =
  (,) <$> wP <* string " to "
    <*> ((,) <$> wP <* string " = "
           <*> decimal1P)
  where
    wP = munch1 isLetter

dfs :: M.Map String [(String, Int)] -> String -> Int -> S.Set String -> [Int]
dfs g cur dist todos =
  if S.null todos
    then pure dist
    else do
      Just nexts <- pure $ g M.!? cur
      (next, d) <- nexts
      guard $ S.member next todos
      dfs g next (dist + d) $ S.delete next todos

instance Solution Day9 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    es <- fmap (consumeOrDie edgeP) . lines <$> getInputS
    let g :: M.Map String [(String, Int)]
        g = M.fromListWith (<>) do
          (u, (v, d)) <- es
          [(u, [(v, d)]), (v, [(u, d)])]
        ns = S.fromList do
          (u, (v, _)) <- es
          [u, v]
        dists = do
          n <- S.toList ns
          dfs g n 0 (S.delete n ns)
        Just (MinMax (ans1, ans2)) = foldMap (Just . minMax) dists
    answerShow ans1
    answerShow ans2
