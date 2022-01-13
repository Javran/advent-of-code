{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2017.Day12
  (
  )
where

import Control.Monad
import Control.Monad.ST
import qualified Data.Map.Strict as M
import qualified Data.IntSet as IS
import qualified Javran.AdventOfCode.UnionFind.ST as UF
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day12 deriving (Generic)

type CommInfo = (Int, [Int])

commInfoP :: ReadP CommInfo
commInfoP =
  (,) <$> (decimal1P <* string " <-> ")
    <*> (decimal1P `sepBy1` string ", ")

solve :: IS.IntSet -> [CommInfo] -> ST s (Int, Int)
solve pgs cs = do
  let ps = IS.toAscList pgs
  pts <- mapM (\i -> UF.fresh (IS.singleton i)) ps
  let pgMap = M.fromList $ zip ps pts
  forM_ cs \(p, qs) ->
    forM_ qs \q -> do
      -- choose to compute pRep in inner loop,
      -- as repr might change due to compression.
      pRep <- UF.repr (pgMap M.! p)
      qRep <- UF.repr (pgMap M.! q)
      UF.union' pRep qRep (\x y -> pure $ IS.union x y)
  let p0 = pgMap M.! 0
  s <- UF.descriptor p0
  ccnt <- UF.countClusters pts
  pure (IS.size s, ccnt)

instance Solution Day12 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (consumeOrDie commInfoP) . lines <$> getInputS
    let pgs = IS.fromList (fmap fst xs)
        (ans1, ans2) = runST $ solve pgs xs
    answerShow ans1
    answerShow ans2
