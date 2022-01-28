{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2015.Day8
  (
  )
where

import Control.Applicative
import Control.Monad
import Data.Char
import Javran.AdventOfCode.Prelude
import Numeric
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day8 deriving (Generic)

strLitP :: ReadP String
strLitP = between (char '"') (char '"') do
  concat <$> many (simpleP <++ escapedP)
  where
    simpleP = munch1 (`notElem` "\\\"")
    escapedP =
      char '\\' *> do
        (['\\'] <$ char '\\')
          <++ (['\"'] <$ char '\"')
          <++ do
            charP 'x'
            xs <- replicateM 2 (satisfy isHexDigit)
            [(v, "")] <- pure $ readHex xs
            pure [chr v]

instance Solution Day8 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- lines <$> getInputS
    do
      let process x = length x - length (consumeOrDie strLitP x)
      answerShow $ sum (fmap process xs)
    do
      let process x = length (show x) - length x
      answerShow $ sum (fmap process xs)
