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
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Set as S
import qualified Data.Vector as V
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
  , ( MinMax2D Int Int {- TODO: it's probably not necessary to have this bound. -}
    , Bool {- delayed negation flag -}
    )
  )

bitsToDigit :: [Bool] -> Int
bitsToDigit = foldl' (\acc i -> acc * 2 + bool 0 1 i) 0

stepRange :: Enum e => (e, e) -> (e, e)
stepRange = bimap pred succ

{-
  The tricky bit of this puzzle is when Rule[0] = 1,
  in which case all zeros results in one -
  In this situation we can no longer just store the set of coordinates of ones,
  as it's infinitely many.

  To deal with that, we have a negation flag, flip it every turn.
  to get the actual value, we also look at this flag and see if
  we want to flip what we have read.

  Also a note that, when Rule[0] = 1, we also relies on
  Rule[511] = 0 so we can flip back right next turn.
  This actually must be true, otherwise all ones will stay on ones,
  resulting in infinitely many ones,
  which cannot be a finite answer to this puzzle.
 -}
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
