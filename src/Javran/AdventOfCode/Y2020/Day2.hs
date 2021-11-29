{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2020.Day2
  ( main
  )
where

import Data.Char
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

isValidLine :: String -> Bool
isValidLine xs = case readP_to_S
  ((,)
     <$> validatorP
     <*> (munch (const True) <* eof))
  xs of
  [((fSpec, v), "")] -> buildValidator fSpec v
  _ -> False

main :: IO ()
main = do
  rawLines <- lines <$> getInput @String 2020 2
  print . length . filter id . fmap isValidLine $ rawLines
  pure ()
