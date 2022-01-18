{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module Javran.AdventOfCode.Y2017.Day16
  (
  )
where

import Control.Lens
import Control.Monad
import Data.Char
import Data.Coerce
import Data.List
import qualified Data.Map.Strict as M
import Data.Monoid
import Javran.AdventOfCode.Misc (rotateRightBy)
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.TestExtra
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day16 deriving (Generic)

newtype Prog = Prog Int deriving (Show, Eq, Ord)

data Move
  = Spin Int
  | Exchange Int Int
  | Partner Prog Prog
  deriving (Show)

moveP :: ReadP Move
moveP = spinP <++ exchangeP <++ partnerP
  where
    spinP = char 's' *> (Spin <$> decimal1P)
    exchangeP = char 'x' *> (Exchange <$> (decimal1P <* char '/') <*> decimal1P)
    partnerP = char 'p' *> (Partner <$> (progP <* char '/') <*> progP)
    progP :: ReadP Prog
    progP = do
      v <- satisfy (\ch -> ch >= 'a' && ch <= 'p')
      pure $ Prog $ fromIntegral $ ord v - ord 'a'

pprPrograms :: Programs -> String
pprPrograms = fmap render
  where
    render (Prog v) = chr $ ord 'a' + v

{-
  TODO: now we've seen this a bunch of times:

  - Y2017.Day6
  - Y2017.Day16
  - Y2018.Day18 (in which we need to speed up the function same way)
  - Y2018.Day21
  - Y2020.Day11

  Probably this could be some prelude function.
 -}
findFix :: Ord a => M.Map a Int -> [(Int, a)] -> (Int, Int)
findFix seen ~((j, x) : xs) = case seen M.!? x of
  Just i -> (i, j)
  Nothing -> findFix (M.insert x j seen) xs

type Programs = [Prog]

mkPrograms :: Int -> Programs
mkPrograms n = coerce [0 .. n -1]

spin :: Int -> Int -> Programs -> Programs
spin n d = rotateRightBy n d

exchange :: Int -> Int -> Programs -> Programs
exchange i j xs = xs & ix i .~ pJ & ix j .~ pI
  where
    pI = xs !! i
    pJ = xs !! j

partner :: Prog -> Prog -> Programs -> Programs
partner pI pJ xs = xs & ix i .~ pJ & ix j .~ pI
  where
    tracedXs = zip [0 ..] xs
    -- TODO: this probably worth moving to prelude.
    firstMatch needle (ind, x) = Alt $ ind <$ guard (needle == x)
    (Alt (Just i), Alt (Just j)) =
      foldMap (\x' -> (firstMatch pI x', firstMatch pJ x')) tracedXs

applyMove :: Int -> Move -> Programs -> Programs
applyMove n = \case
  Spin v -> spin n v
  Exchange i j -> exchange i j
  Partner i j -> partner i j

{-
  Note for part 2: At first this seems to be just a permutation
  that we can speed up through matrix multiplication, which isn't actually the case.
  In particular, operation `Partner` doesn't do a consistent permutation as what it does
  depends on where those elements are.

  I don't think simulation all the way is gonna fly, so we'll probably looking
  at finding a fixpoint.
 -}

instance Solution Day16 where
  solutionRun _ SolutionContext {getInputS, answerS} = do
    (extraOps, rawInput) <- consumeExtra getInputS
    let xs = consumeOrDie ((moveP `sepBy` char ',') <* char '\n') rawInput
        n = case extraOps of
          Just ~[rawN] -> read rawN
          Nothing -> 16
        s0 = mkPrograms n
        applyAll z = foldl' (\cur move -> applyMove n move cur) z xs
        progression = iterate applyAll s0
    answerS $ pprPrograms (progression !! 1)
    do
      let (firstI, dupI) = findFix mempty (zip [0 ..] progression)
          period = dupI - firstI
          r = (1_000_000_000 - firstI) `rem` period
      answerS $ pprPrograms $ progression !! r
