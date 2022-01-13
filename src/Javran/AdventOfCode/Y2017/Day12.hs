{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2017.Day12
  (
  )
where

import Control.Monad
import Control.Monad.ST
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.UnionFind.ST as UF
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day12 deriving (Generic)

type CommInfo = (Int, [Int])

commInfoP :: ReadP CommInfo
commInfoP =
  (,) <$> (decimal1P <* string " <-> ")
    <*> (decimal1P `sepBy1` string ", ")

-- https://github.com/nominolo/union-find/issues/12#issuecomment-647090797
cluster :: [(UF.Point s Int, b)] -> ST s (IM.IntMap [b])
cluster pairs =
  IM.map ($ []) . IM.fromListWith (flip (.))
    <$> mapM
      (\(uf, c) -> do
         cRep <- UF.descriptor =<< UF.repr uf
         pure (cRep, (c :)))
      pairs

solve :: IS.IntSet -> [CommInfo] -> ST s (Int, IM.IntMap [Int])
solve pgs cs = do
  let ps = IS.toAscList pgs
  pts <- mapM UF.fresh ps
  let pgMap = IM.fromList $ zip ps pts
  forM_ cs \(p, qs) ->
    forM_ qs \q -> do
      -- choose to compute pRep in inner loop,
      -- as repr might change due to compression.
      pRep <- UF.repr (pgMap IM.! p)
      qRep <- UF.repr (pgMap IM.! q)
      unless (pRep == qRep) do
        UF.union pRep qRep
  let p0 = pgMap IM.! 0
  cnt <- mapM (UF.equivalent p0) (IM.elems pgMap)
  c <- cluster $ zip pts ps
  pure (countLength id cnt, c)

instance Solution Day12 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (consumeOrDie commInfoP) . lines <$> getInputS
    let pgs = IS.fromList (fmap fst xs)
        (ans1, clusters) = runST $ solve pgs xs
    answerShow ans1
    answerShow (IM.size clusters)
