{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Javran.AdventOfCode.Y2021.Day8
  (
  )
where

import Data.Bifunctor
import qualified Data.IntMap.Strict as IM
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Tuple
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude

data Day8 deriving (Generic)

type Pat = S.Set Char

wireMap :: M.Map Pat Int
wireMap =
  M.fromList $
    (fmap . first)
      S.fromList
      [ ("abcefg", 0)
      , ("cf", 1)
      , ("acdeg", 2)
      , ("acdfg", 3)
      , ("bcdf", 4)
      , ("abdfg", 5)
      , ("abdefg", 6)
      , ("acf", 7)
      , ("abcdefg", 8)
      , ("abcdfg", 9)
      ]

solve :: [String] -> [] Char
solve ds = [a, b, c, d, e, f, g]
  where
    [g] = mustBeDG \\ [d]
    [d] = mustBeBCDF \\ [b, c, f]
    [c] = mustBeCF \\ [f]
    [a] = mustBeACF \\ mustBeCF

    {-
      if we count how many times each letter appear in all 10 digits
      and use the frequency as key:

      - 4: {e}
      - 6: {b}
      - 7: {d,g}
      - 8: {a,c}
      - 9: {f}

      this together with clues from digits is sufficient
      to derive the unique solution.
     -}
    [ (4, [e])
      , (6, [b])
      , (7, mustBeDG)
      , (8, _mustBeAC)
      , (9, [f])
      ] = IM.toAscList $ IM.fromListWith (<>) do
        (cnt, ch) <- fmap swap freqCount
        pure (cnt, [ch])
    freqCount =
      M.toAscList . M.fromListWith (+) . fmap (,1) . concat $ ds

    -- clues from '1' '4' '7'
    [mustBeCF] = lMap IM.! 2 -- 1
    [mustBeBCDF] = lMap IM.! 4 -- 4
    [mustBeACF] = lMap IM.! 3 -- 7
    lMap = IM.fromListWith (<>) $ fmap (\v -> (length v, [v])) ds

instance Solution Day8 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    let parseLine raw =
          let [x, y] = splitOn ["|"] . words $ raw
           in (x, y)
    xs <- fmap parseLine . lines <$> getInputS
    answerShow $
      countLength (`elem` [2, 4, 3, 7]) $
        fmap length $ concatMap snd xs
    answerShow $
      sum $ do
        (ls, rs) <- xs
        let [a, b, c, d, e, f, g] = solve ls
            m = M.fromList $ zip [a, b, c, d, e, f, g] ['a' .. 'g']
            translate =
              (wireMap M.!)
                . S.fromList
                . fmap (m M.!)
            digits = fmap translate rs
        pure (foldl (\acc i -> acc * 10 + i) 0 digits)
