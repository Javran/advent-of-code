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
import Javran.AdventOfCode.UnionFind.ST (snapshotClusters)

data Day12 deriving (Generic)

type CommInfo = (Int, [Int])

commInfoP :: ReadP CommInfo
commInfoP =
  (,) <$> (decimal1P <* string " <-> ")
    <*> (decimal1P `sepBy1` string ", ")

solve :: IS.IntSet -> [CommInfo] -> ST s (Int, M.Map Int [Int])
solve pgs cs = do
  let ps = IS.toAscList pgs
  pts <- mapM UF.fresh ps
  let pgMap = M.fromList $ zip ps pts
  forM_ cs \(p, qs) ->
    forM_ qs \q -> do
      -- choose to compute pRep in inner loop,
      -- as repr might change due to compression.
      pRep <- UF.repr (pgMap M.! p)
      qRep <- UF.repr (pgMap M.! q)
      UF.union pRep qRep
  let p0 = pgMap M.! 0
  cnt <- mapM (UF.equivalent p0) (M.elems pgMap)
  c <- snapshotClusters $ zip pts ps
  pure (countLength id cnt, c)

instance Solution Day12 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (consumeOrDie commInfoP) . lines <$> getInputS
    let pgs = IS.fromList (fmap fst xs)
        (ans1, clusters) = runST $ solve pgs xs
    answerShow ans1
    answerShow (M.size clusters)
