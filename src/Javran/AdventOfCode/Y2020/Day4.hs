{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2020.Day4
  (
  )
where

import Control.Monad
import Data.Char
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP
import Javran.AdventOfCode.Misc (commitLeft1)

type Record = S.Set String

data Day4 deriving (Generic)

instance Solution Day4 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    raw <- getInputS
    let rawRecords = fmap toRawRecord . splitOn [""] . lines $ raw
        validRawRecords = filter (isValid . S.fromList . fmap fst) rawRecords
    answerShow $ length validRawRecords
    answerShow $ countLength isValid2 validRawRecords

requiredFields :: S.Set String
requiredFields = S.fromList $ words "byr iyr eyr hgt hcl ecl pid" -- cid is optional

valueValidators :: M.Map String (String -> Bool)
valueValidators =
  M.fromList
    (second (\p xs -> isJust (consumeAllWithReadP p xs))
       <$> [ ("byr", fourDigits 1920 2002)
           , ("iyr", fourDigits 2010 2020)
           , ("eyr", fourDigits 2020 2030)
           , ( "hgt"
             , do
                 v <- decimal1P @Int
                 let cmP = guard $ v >= 150 && v <= 193
                     inP = guard $ v >= 59 && v <= 76
                 (string "cm" *> cmP) <++ (string "in" *> inP)
             )
           , ( "hcl"
             , char '#'
                 *> replicateM_
                   6
                   (satisfy (`elem` (['0' .. '9'] <> ['a' .. 'f'])))
             )
           , ("ecl", commitLeft1 $ strP <$> words "amb blu brn gry grn hzl oth")
           , ( "pid"
             , replicateM_ 9 (satisfy isDigit)
             )
           ])
  where
    -- a bit subtle: decimal1P accepts 01920 while this does not.
    fourDigits lo hi = do
      xs <- replicateM 4 (satisfy isDigit)
      let v = read @Int xs
      guard $ v >= lo && v <= hi

toRawRecord :: [String] -> [(String, String)]
toRawRecord recLines = do
  curLine <- recLines
  rawKvPair <- words curLine
  let (k, ':' : v) = span (/= ':') rawKvPair
  pure (k, v)

isValid :: Record -> Bool
isValid r = null $ S.difference requiredFields r

isValid2 :: [(String, String)] -> Bool
isValid2 kvs = isJust $ mapM_ verifyKv kvs
  where
    verifyKv (k, v) = case valueValidators M.!? k of
      Nothing -> pure ()
      Just validator -> guard $ validator v
