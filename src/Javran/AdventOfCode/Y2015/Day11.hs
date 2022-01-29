{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2015.Day11
  (
  )
where

import Control.Monad
import Data.Char
import Data.List
import Data.List.Split hiding (sepBy)
import Data.Semigroup
import Javran.AdventOfCode.Prelude

data Day11 deriving (Generic)

{-
  The representation is reversed so that we can increase from
  the least significant end.
 -}
type RevPass = String

bans :: [] Char
bans = "iol"

incr :: RevPass -> RevPass
incr xs = case mostSignificantBan of
  Nothing -> incrStd xs
  Just (Last (i, x)) ->
    {-
      Optimization: if there is already banned digits in it,
      tick the most significant digit containing banned digit,
      and fill lower digits with 'a'
     -}
    replicate i 'a' <> (succ x : drop (i + 1) xs)
  where
    mostSignificantBan =
      foldMap
        (\p@(_, x) -> Last p <$ guard (x `elem` bans))
        (zip [0 ..] xs)

incrStd :: RevPass -> RevPass
incrStd = \case
  [] -> []
  c : ch ->
    if
        | c == 'z' -> 'a' : incrStd ch
        | c `elem` preBans ->
          let c' = chr (ord c + 2)
           in if not checked || c' <= 'z'
                then c' : ch
                else error "resulting value out of range"
        | otherwise -> chr (ord c + 1) : ch
  where
    checked = False
    preBans = fmap pred bans

isCompliant :: RevPass -> Bool
isCompliant xs =
  not (null twoPairs)
    && not (null incrStraight)
    && not (any (`elem` bans) xs)
  where
    incrStraight = do
      ~[c, b, a] <- divvy 3 1 xs
      guard $ [a, b, c] == take 3 [a ..]
    twoPairs = do
      ts <- tails xs
      ([a, b], ys) <- pure $ splitAt 2 ts
      guard $ a == b
      guard $ any (\[c, d] -> c == d) $ divvy 2 1 ys

instance Solution Day11 where
  solutionRun _ SolutionContext {getInputS, answerS} = do
    xs <- reverse . head . lines <$> getInputS
    let ans1 : ans2 : _ = filter isCompliant $ tail $ iterate incr xs
    answerS $ reverse ans1
    answerS $ reverse ans2
