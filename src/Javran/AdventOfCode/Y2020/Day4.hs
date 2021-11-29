{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2020.Day4
  ( main
  )
where

import qualified Data.List.Split as LSplit
import qualified Data.Set as S
import Javran.AdventOfCode.Prelude

type Record = S.Set String

requiredFields :: S.Set String
requiredFields = S.fromList $ words "byr iyr eyr hgt hcl ecl pid" -- cid is optional

toRecord :: [String] -> Record
toRecord recLines = S.fromList $ do
  curLine <- recLines
  rawKvPair <- words curLine
  pure $ takeWhile (/= ':') rawKvPair

isValid :: Record -> Bool
isValid r = null $ S.difference requiredFields r

main :: IO ()
main = do
  raw <- getInput @String 2020 4
  let rs = fmap toRecord . LSplit.splitOn [""] . lines $ raw
  print $ length $ filter isValid rs
