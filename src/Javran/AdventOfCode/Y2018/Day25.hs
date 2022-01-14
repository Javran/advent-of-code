{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2018.Day25
  (
  )
where

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector as V
import Javran.AdventOfCode.Prelude
import qualified Javran.AdventOfCode.UnionFind.ST as UF
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day25 deriving (Generic)

type Pt = (Int, Int, Int, Int)

ptP :: ReadP Pt
ptP = do
  [a, b, c, d] <- intP `sepBy1` char ','
  pure (a, b, c, d)
  where
    intP = readS_to_P (reads @Int)

countConstellations :: [Pt] -> ST s Int
countConstellations xs = do
  let pts = V.fromList xs
  ss <- V.fromListN (V.length pts) <$> mapM UF.fresh xs
  forM_
    (do
       (i, xs0) <- pickInOrder [0 .. V.length pts - 1]
       j <- xs0
       pure (i, j))
    $ \(i, j) -> do
      when
        (manhattan (pts V.! i) (pts V.! j) <= 3)
        do
          UF.union (ss V.! i) (ss V.! j)
  UF.countClusters ss

instance Solution Day25 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (consumeOrDie ptP) . lines <$> getInputS
    answerShow $ runST $ countConstellations xs
