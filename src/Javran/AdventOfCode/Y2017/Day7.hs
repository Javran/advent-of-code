{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Javran.AdventOfCode.Y2017.Day7
  (
  )
where

import qualified Data.Array as Arr
import Data.Char
import Data.Function
import Data.Function.Memoize (memoFix)
import Data.Graph
import Data.List
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day7 deriving (Generic)

{- element orders are arranaged so that this can be input for `graphFromEdges`. -}
type Prog = (Int, String, [String])

progP :: ReadP Prog
progP = do
  let nameP = munch1 isAlpha
  pg <- nameP <* string " ("
  w <- decimal1P <* char ')'
  xs <-
    (string " -> " *> (nameP `sepBy1` string ", "))
      <++ pure []
  pure (w, pg, xs)

{-
  Finds the unique value, returns its index and diffs against others.

  would throw if assumptions are broken.
 -}
findImbalance :: forall e. e ~ Int => [(Int, e)] -> Maybe (Int, e)
findImbalance xs = case groupped of
  [_] -> Nothing
  [ls, rs] ->
    case (ls, rs) of
      ([(i, x)], (_, y) : _ : _) ->
        -- unique val on left, right contains at least 2
        Just (i, y - x)
      ((_, y) : _ : _, [(i, x)]) ->
        -- unique val on right, left contains at least 2
        Just (i, y - x)
      _ -> error "no unique value present."
  _ -> error "supposed to have 1 or 2 distinct values."
  where
    groupped = groupBy ((==) `on` snd) $ sortOn snd xs

instance Solution Day7 where
  solutionRun _ SolutionContext {getInputS, answerShow, answerS} = do
    es <- fmap (consumeOrDie progP) . lines <$> getInputS
    let (graph, nodeFromVertex, _vertexFromKey) = graphFromEdges es
        getWeightSum :: Vertex -> Int
        getWeightSum = memoFix \q v ->
          let (w, _, _) = nodeFromVertex v in w + sum (fmap q (graph Arr.! v))
        rootV : _ = topSort graph
        childrenWeights v = fmap getWeightSum (graph Arr.! v)
        {-
          INVARIANT: the node with wrong weight is always inside v (including v itself).

          Traces the only imbalanced subtree. If we can't find one,
          it must be the case that v itself has the wrong weight.
         -}
        findWrongWeight v wDiff = case findImbalance $ zip [0 ..] (childrenWeights v) of
          Nothing -> let (w, _, _) = nodeFromVertex v in w + wDiff
          Just (ind, wDiff') ->
            findWrongWeight (graph Arr.! v !! ind) wDiff'

    answerS (let (_, n, _) = nodeFromVertex $ head $ topSort graph in n)
    answerShow $ findWrongWeight rootV (error "no imbalances found")
