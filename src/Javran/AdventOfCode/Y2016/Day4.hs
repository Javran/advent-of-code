{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2016.Day4
  (
  )
where

import Control.Monad
import Data.Char
import Data.List
import qualified Data.Map.Strict as M
import Data.Semigroup
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.TestExtra
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day4 deriving (Generic)

type EncName = (([String], Int), String)

encNameP :: ReadP EncName
encNameP = do
  let chunkP = munch1 isAsciiLower
  cs <- many1 (chunkP <* char '-')
  v <- decimal1P
  chk <- between (char '[') (char ']') chunkP
  pure ((cs, v), chk)

verifyRoom :: EncName -> Maybe Int
verifyRoom ((chunks, sec), chk) = sec <$ guard (and (zipWith (==) chk freq))
  where
    freq = fmap fst $
      sortOn (\(ch, fq) -> (Down fq, ch)) $
        M.toList $ M.fromListWith (+) do
          c <- concat chunks
          pure (c, 1 :: Int)

decryptBy :: Int -> Char -> Char
decryptBy sec ch = chr (ord 'a' + v')
  where
    v = ord ch - ord 'a'
    v' = (v + sec) `rem` 26

decryptName :: EncName -> String
decryptName ((chunks, sec), _) = unwords $ fmap (fmap (decryptBy sec)) chunks

instance Solution Day4 where
  solutionRun _ SolutionContext {getInputS, answerShow, answerS} = do
    (extraOps, rawInput) <- consumeExtra getInputS
    let xs = fmap (consumeOrDie encNameP) . lines $ rawInput
        processRoom en = do
          sec <- verifyRoom en
          let realName = decryptName en
              m :: Maybe (First Int)
              m = do
                guard $ realName == "northpole object storage"
                pure $ First sec
          pure ((sec, realName), m)
        rooms = mapMaybe processRoom xs
    answerShow $ sum (fmap (fst . fst) rooms)
    case extraOps of
      Nothing -> do
        let Just (First ans) = foldMap snd rooms
        answerShow ans
      Just _ ->
        mapM_ (answerS . snd . fst) rooms
