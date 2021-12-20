{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2021.Day20
  (
  )
where

import Control.Monad
import Data.Bifunctor
import Data.Bool
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Set as S
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude

data Day20 deriving (Generic)

type Rule = V.Vector Bool

encodeRule :: String -> Rule
encodeRule xs = case length xs of
  512 -> V.fromListN 512 $ fmap (== '#') xs
  l -> error $ "unexpected encode rule length: " <> show l

type Coord = (Int, Int)

type World =
  ( S.Set Coord
  , (MinMax2D Int Int, Bool {- delayed negation flag -})
  )

bitsToDigit :: [Bool] -> Int
bitsToDigit = foldl' (\acc i -> acc * 2 + bool 0 1 i) 0

stepRange :: Enum e => (e, e) -> (e, e)
stepRange = bimap pred succ

mkStep :: Rule -> World -> World
mkStep rule (xs, (MinMax2D wRange, negative)) =
  (xs', (MinMax2D wRange', negative'))
  where
    wRange'@(rRange', cRange') = bimap stepRange stepRange wRange
    needNegative = rule V.! 0
    negative' = if needNegative then not negative else negative
    xs' = S.fromList do
      coord@(r, c) <-
        let f = uncurry enumFromTo
         in (,) <$> f rRange' <*> f cRange'
      let inp = bitsToDigit do
            r' <- [r -1 .. r + 1]
            c' <- [c -1 .. c + 1]
            pure $
              -- apply negation to get the actual input value
              (if negative then S.notMember else S.member) (r', c') xs
      let actualVal = rule V.! inp
      guard $
        -- whether output is supposed to be negated.
        if negative' then not actualVal else actualVal
      pure coord

showWorld :: World -> IO ()
showWorld (xs, (MinMax2D ((minR, maxR), (minC, maxC)), negative)) = do
  when negative do
    putStrLn "Showing negative of the actual."
  forM_ [minR .. maxR] \r -> do
    let render c = bool "  " "██" $ S.member (r, c) xs
    putStrLn (concatMap render [minC .. maxC])

instance Solution Day20 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (extraOps, rawInput) <- consumeExtraLeadingLines <$> getInputS
    let [[xs], ys] = splitOn [""] . lines $ rawInput
        rows = length ys
        cols = length (head ys)
        rule = encodeRule xs
        initLit = S.fromList do
          (r, rs) <- zip [0 ..] ys
          (c, '#') <- zip [0 ..] rs
          pure (r, c)
        initWd :: World
        initWd = (initLit, (MinMax2D ((0, rows -1), (0, cols -1)), False))
        step = mkStep rule
        debug = False
    let progression = iterate step initWd
        answer i = answerShow . S.size . fst $ progression !! i
        atSteps = case extraOps of
          Just ss -> fmap (read @Int) ss
          Nothing -> [2, 50]
    when debug do
      showWorld initWd
    mapM_ answer atSteps
