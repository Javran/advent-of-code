{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Javran.AdventOfCode.Y2018.Day2
  (
  )
where


import Control.Monad
import qualified Data.Map.Strict as M
import Data.Monoid
import Javran.AdventOfCode.Prelude

data Day2 deriving (Generic)

toCount :: String -> (Sum Int, Sum Int)
toCount xs = (bool 0 1 $ 2 `elem` freqs, bool 0 1 $ 3 `elem` freqs)
  where
    freqs = M.elems . M.fromListWith (+) . fmap (,1 :: Int) $ xs

countDiff :: String -> String -> Int
countDiff xs ys = getSum $ mconcat $ zipWith (\x y -> bool 0 1 (x /= y)) xs ys

instance Solution Day2 where
  solutionRun _ SolutionContext {getInputS, answerShow, answerS} = do
    xs <- lines <$> getInputS
    answerShow $ let (Sum u, Sum v) = foldMap toCount xs in u * v
    answerS $ head do
      (y0, ys0) <- pickInOrder xs
      (y1, _) <- pickInOrder ys0
      guard $ countDiff y0 y1 == 1
      pure $ concat $ zipWith (\x y -> bool [] [x] (x == y)) y0 y1
