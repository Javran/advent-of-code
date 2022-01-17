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
import Data.Fix
import Data.Functor.Foldable
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day9 deriving (Generic)

data ChunkF r
  = Plain String
  | Repeated Int r
  deriving (Show, Functor)

type SimpleChunk = ChunkF String

type Chunk = Fix ChunkF

chunkP :: ReadP SimpleChunk
chunkP = repeatedP <++ (Plain <$> munch1 (`notElem` "(\n"))
  where
    repeatedP = do
      (m, n) <- between (char '(') (char ')') do
        m <- decimal1P
        _ <- char 'x'
        n <- decimal1P
        pure (m, n)
      xs <- replicateM m nextCharP
      pure $ Repeated n xs

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
