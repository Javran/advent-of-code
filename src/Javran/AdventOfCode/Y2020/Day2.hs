{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2020.Day2
  ( main
  )
where

import Data.Char
import Data.Maybe
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP

type ValidatorSpec = ((Int, Int), Char)

type Validator = String -> Bool

decimal1 :: (Read i, Integral i) => ReadP i
decimal1 = read <$> munch1 isDigit

buildValidator :: ValidatorSpec -> Validator
buildValidator ((lo, hi), ch) xs =
  let cnt = length (filter (== ch) xs)
   in cnt >= lo && cnt <= hi

validatorP :: ReadP ValidatorSpec
validatorP = do
  lo <- decimal1
  _ <- char '-'
  hi <- decimal1
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

main :: IO ()
main = do
  rawLines <- lines <$> getInput @String 2020 2
  print . length . filter id . fmap (isValidLine buildValidator) $ rawLines
  print . length . filter id . fmap (isValidLine buildValidator2) $ rawLines
  pure ()
