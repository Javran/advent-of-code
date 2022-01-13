module Javran.AdventOfCode.UnionFind.ST
  ( snapshotClusters
  , countClusters
  , module Data.UnionFind.ST
  )
where

import Control.Monad.ST
import qualified Data.Map.Strict as M
import Data.UnionFind.ST
import Javran.AdventOfCode.Prelude

{-
  Creates a snapshot of current clustering.

  Every point are supposed to be paired with a value that
  could identify the original element. This is because,
  as we perform unions, information gets destroyed so we
  need some extra help to recover them.

  I commented about this originally in:

  https://github.com/nominolo/union-find/issues/12#issuecomment-647090797

  TODO: come to think of it - we can probably use Set as
  descriptor and `union'` might now be useful.

  TODO: interface to use other maps?

  TODO: probably just use DList.

 -}
snapshotClusters :: Ord k => [(Point s k, a)] -> ST s (M.Map k [a])
snapshotClusters pairs =
  fmap ($ []) . M.fromListWith (flip (.))
    <$> mapM
      (\(uf, c) -> do
         cRep <- descriptor =<< repr uf
         pure (cRep, (c :)))
      pairs

countClusters :: Traversable f => f (Point s a) -> ST s Int
countClusters pts =
  countLength not <$> mapM redundant pts
