{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Javran.AdventOfCode.Y2020.Day2
  (
  )
where

import Data.Maybe
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP

data Day2

instance Solution Day2 where
  solutionIndex _ = (2020, 2)
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    rawLines <- lines <$> getInputS
    answerShow . length . filter id . fmap (isValidLine buildValidator) $ rawLines
    answerShow . length . filter id . fmap (isValidLine buildValidator2) $ rawLines

type ValidatorSpec = ((Int, Int), Char)

type Validator = String -> Bool

buildValidator :: ValidatorSpec -> Validator
buildValidator ((lo, hi), ch) xs =
  let cnt = length (filter (== ch) xs)
   in cnt >= lo && cnt <= hi

validatorP :: ReadP ValidatorSpec
validatorP = do
  lo <- decimal1P
  _ <- char '-'
  hi <- decimal1P
  _ <- char ' '
  c <- get
  _ <- string ": "
  pure ((lo, hi), c)

buildValidator2 :: ValidatorSpec -> Validator
buildValidator2 ((pos0, pos1), ch) xs =
  ch0 /= ch1 && ch `elem` catMaybes [ch0, ch1]
  where
    ch0 = getPos pos0
    ch1 = getPos pos1
    l = length xs
    getPos i =
      if i <= l then Just (xs !! (i -1)) else Nothing

isValidLine :: (ValidatorSpec -> Validator) -> Validator
isValidLine fromSpec xs = case readP_to_S
  ((,)
     <$> validatorP
     <*> (munch (const True) <* eof))
  xs of
  [((fSpec, v), "")] -> fromSpec fSpec v
  _ -> False
