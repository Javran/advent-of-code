{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2015.Day1
  (
  )
where

import Control.Applicative
import Data.List
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day1 deriving (Generic)

dirP :: ReadP Int
dirP = (1 <$ char '(') <++ ((-1) <$ char ')')

instance Solution Day1 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- consumeOrDie (many dirP) . head . lines <$> getInputS
    answerShow (sum xs)
    let (ans2, _) : _ =
          dropWhile ((/= -1) . snd) $
            zip [0 :: Int ..] $ scanl' (+) 0 xs
    answerShow ans2
