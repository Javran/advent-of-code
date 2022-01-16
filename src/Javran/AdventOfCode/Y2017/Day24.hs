{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Javran.AdventOfCode.Y2017.Day24
  (
  )
where

import Control.Monad
import Data.Semigroup
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day24 deriving (Generic)

type Conn = (Int, Int)

connP :: ReadP Conn
connP = (,) <$> (decimal1P <* char '/') <*> decimal1P

dfs :: [Conn] -> Int -> Int -> Int -> [(Int, Int)]
dfs conns cur strength len = do
  let alts = do
        (cPre@(u, v), conns') <- pick conns
        c@(l, _) <- if u == v then [cPre] else [cPre, (v, u)]
        guard $ l == cur
        pure (c, conns')
  case alts of
    [] -> pure (strength, len)
    _ : _ -> do
      ((l, r), conns') <- alts
      dfs conns' r (strength + l + r) (len + 1)

instance Solution Day24 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    conns <- fmap (consumeOrDie connP) . lines <$> getInputS
    let solutions = dfs conns 0 0 0
        Just (Max ans1, Max (_, ans2)) =
          foldMap (\(st, len) -> Just (Max st, Max (len, st))) solutions

    mapM_ answerShow [ans1, ans2]
