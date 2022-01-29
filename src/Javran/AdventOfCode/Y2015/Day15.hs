{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2015.Day15
  (
  )
where

import Control.Monad
import Data.Char
import Data.Monoid
import Data.Semigroup
import Data.Semigroup.Generic
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day15 deriving (Generic)

data Ingredient a = Ingredient
  { gCapa :: a
  , gDura :: a
  , gFlav :: a
  , gText :: a
  , gCalo :: a
  }
  deriving stock (Show, Generic)
  deriving
    (Semigroup, Monoid)
    via (GenericSemigroupMonoid (Ingredient (Sum a)))

ingredientP :: ReadP (String, Ingredient Int)
ingredientP = do
  let intP = readS_to_P (reads @Int)
  n <- munch1 isLetter <* strP ": "
  gCapa <- strP "capacity " *> intP <* strP ", "
  gDura <- strP "durability " *> intP <* strP ", "
  gFlav <- strP "flavor " *> intP <* strP ", "
  gText <- strP "texture " *> intP <* strP ", "
  gCalo <- strP "calories " *> intP
  pure (n, Ingredient {gCapa, gDura, gFlav, gText, gCalo})

{-
  Partitions a positive integer `total` into `n` non-negative integers.
 -}
intPart :: Int -> Int -> [] [Int]
intPart n total
  | n <= 0 = errInvalid
  | n == 1 = pure [total]
  | otherwise = do
    v <- [0 .. total]
    ps <- intPart (n -1) (total - v)
    pure $ v : ps

instance Solution Day15 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    ingredients <- fmap (consumeOrDie ingredientP) . lines <$> getInputS
    let searchSpace = intPart (length ingredients) 100
        score :: [Int] -> (Int, Bool)
        score = f . mconcat . zipWith (\(_, g) cnt -> stimesMonoid cnt g) ingredients
          where
            f Ingredient {gCapa, gDura, gFlav, gText, gCalo} =
              (product $ fmap (max 0) [gCapa, gDura, gFlav, gText], gCalo == 500)
        (Just (Max ans1), Just (Max ans2)) = foldMap f searchSpace
          where
            f counts = (Just (Max v), Max v <$ guard sat)
              where
                (v, sat) = score counts
    answerShow ans1
    answerShow ans2
