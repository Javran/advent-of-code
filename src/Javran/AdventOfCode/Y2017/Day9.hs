{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Javran.AdventOfCode.Y2017.Day9
  (
  )
where

import Control.Applicative
import Control.Monad.Writer.CPS
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day9 deriving (Generic)

data Thing = Group [Thing] | Garbage (Sum Int) deriving (Show)

garbageP :: ReadP Thing
garbageP =
  Garbage
    <$> between
      (char '<')
      (char '>')
      (sum <$> many (escaped <++ normal))
  where
    escaped = 0 <$ (char '!' >> nextCharP)
    normal = Sum . length <$> munch1 (`notElem` "!>")

groupP :: ReadP Thing
groupP = Group <$> between (char '{') (char '}') (thingP `sepBy` char ',')

thingP :: ReadP Thing
thingP = groupP <++ garbageP

countScore :: Int -> Thing -> Writer (Sum Int, Sum Int) ()
countScore myScore = \case
  Group xs -> do
    tell (Sum myScore, mempty)
    mapM_ (countScore (myScore + 1)) xs
  Garbage n -> tell (mempty, n)

instance Solution Day9 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    v <- consumeOrDie (thingP <* char '\n') <$> getInputS
    let (Sum ans1, Sum ans2) = execWriter $ countScore 1 v
    answerShow ans1
    answerShow ans2
