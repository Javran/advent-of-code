{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Javran.AdventOfCode.Y2021.Day13
  (
  )
where

import Control.Monad
import Data.List.Split hiding (sepBy)
import Data.Semigroup
import qualified Data.Set as S
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, many)

data Day13 deriving (Generic)

type Dot = (Int, Int)

data FoldAlong = AlongX Int | AlongY Int deriving (Show)

foldInstrP :: ReadP FoldAlong
foldInstrP = string "fold along " *>
  ((string "y=" *> (AlongY <$> decimal1P))
    <++ (string "x=" *> (AlongX <$> decimal1P)))

foldAlongX :: Int -> S.Set Dot -> S.Set Dot
foldAlongX hw xs = S.fromList do
  (x, y) <- S.toList xs
  case compare x hw of
    LT -> pure (x, y)
    EQ -> errInvalid
    GT -> pure (hw * 2 - x, y)

foldAlongY :: Int -> S.Set Dot -> S.Set Dot
foldAlongY hh xs = S.fromList do
  (x, y) <- S.toList xs
  case compare y hh of
    LT -> pure (x, y)
    EQ -> errInvalid
    GT -> pure (x, hh * 2 - y)

appFoldInstr :: FoldAlong -> S.Set Dot -> S.Set Dot
appFoldInstr = \case
  AlongX x -> foldAlongX x
  AlongY y -> foldAlongY y

instance Solution Day13 where
  solutionRun _ SolutionContext {getInputS, answerShow, answerS, terminal} = do
    [dotsRaw, foldInstrsRaw] <- splitOn [""] . lines <$> getInputS
    let dots =
          S.fromList $
            fmap ((\[x, y] -> (x, y)) . fmap read . splitOn ",") dotsRaw
        foldInstrs =
          consumeOrDie foldInstrP <$> foldInstrsRaw
    answerShow $ S.size $ appFoldInstr (head foldInstrs) dots
    let result = foldl (flip appFoldInstr) dots foldInstrs
        Just ((Min minX, Max maxX), (Min minY, Max maxY)) =
          foldMap (\(x, y) -> Just ((Min x, Max x), (Min y, Max y))) $
            S.toList result
    forM_ [minY .. maxY] $ \y -> do
      let (ff, tt) = case terminal of
            Nothing -> (".", "#")
            Just _ -> ("  ", "██")
      answerS $
        concatMap
          (\x -> if S.member (x, y) result then tt else ff)
          [minX .. maxX]
