{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2020.Day2
  ( main
  )
where

import Data.Char
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP

type Validator = String -> Bool

decimal1 :: (Read i, Integral i) => ReadP i
decimal1 = read <$> munch1 isDigit

validatorP :: ReadP Validator
validatorP = do
  lo <- decimal1
  _ <- char '-'
  hi <- decimal1
  _ <- char ' '
  c <- get
  _ <- string ": "
  pure $ \xs ->
    let cnt = length (filter (== c) xs)
     in cnt >= lo && cnt <= hi

isValidLine :: String -> Bool
isValidLine xs = case readP_to_S ((,) <$> validatorP <*> (munch (const True) <* eof)) xs of
  [((f, v), "")] -> f v
  _ -> False

main :: IO ()
main = do
  getInput @String 2020 2 >>= print . length . filter id . fmap isValidLine . lines
  pure ()
