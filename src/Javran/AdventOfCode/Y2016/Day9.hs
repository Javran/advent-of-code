{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2016.Day9
  (
  )
where

import Control.Applicative
import Control.Monad
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day9 deriving (Generic)

data ChunkF r
  = Plain String
  | Repeated Int r
  deriving (Show, Functor)

type SimpleChunk = ChunkF String

chunkP :: ReadP SimpleChunk
chunkP = repeatedP <++ (Plain <$> munch1 (`notElem` "(\n"))
  where
    repeatedP = do
      m <- char '(' *> decimal1P
      Repeated <$> (char 'x' *> decimal1P <* char ')')
        <*> replicateM m nextCharP

{-
  TODO: We could use some recursion-schemes stuff here,
  but the problem is we have two layers:

  parseString :: String -> [SimpleChunk]

  in which

  String -> [SimpleChunk]
  ==> String -> [] (ChunkF String)
  ==> String -> (Compose [] ChunkF) String

  so we do have a co-algebra at hand, but there isn't
  a Base (Compose [] ChunkF) that we can use ...
 -}

parseInput :: String -> [SimpleChunk]
parseInput = consumeOrDie (many chunkP <* skipSpaces)

decompressLen :: (r -> Int) -> ChunkF r -> Int
decompressLen rToLen = \case
  Plain xs -> length xs
  Repeated n r -> n * rToLen r

decompressLen2 :: ChunkF String -> Int
decompressLen2 =
  decompressLen (sum . fmap decompressLen2 . parseInput)

instance Solution Day9 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- parseInput <$> getInputS
    answerShow (sum (fmap (decompressLen length) xs))
    answerShow (sum (fmap decompressLen2 xs))
