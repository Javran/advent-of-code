{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2016.Day16
  (
  )
where

import qualified Data.Vector as V
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.TestExtra
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day16 deriving (Generic)

{-
  A virtual sequence.
  consists of only its length and how to access its elements.
 -}
type VSeq = (Int, Int -> Bool)

bitP :: ReadP Bool
bitP = (False <$ char '0') <++ (True <$ char '1')

gen :: VSeq -> VSeq
gen (l, getAt) = (l * 2 + 1, f)
  where
    f i = case compare i l of
      LT -> getAt i
      EQ -> False
      GT -> not (getAt (2 * l - i))

stepChecksum :: VSeq -> VSeq
stepChecksum (l, getAt) =
  ( halve l
  , \i ->
      let i' = i * 2
       in getAt i' == getAt (i' + 1)
  )

computeChecksum :: VSeq -> VSeq
computeChecksum =
  head . dropWhile (even . fst) . tail . iterate stepChecksum

ppr :: VSeq -> String
ppr (l, getAt) = fmap (bool '0' '1' . getAt) [0 .. l -1]

solve :: Int -> VSeq -> VSeq
solve fillLen initSeq = computeChecksum (fillLen, getAt)
  where
    (_, getAt) : _ = dropWhile ((< fillLen) . fst) $ iterate gen initSeq

instance Solution Day16 where
  solutionRun _ SolutionContext {getInputS, answerS} = do
    (ex, rawInput) <- consumeExtra getInputS
    let xs = V.fromList . consumeOrDie (many1 bitP) . head . lines $ rawInput
        initSeq :: VSeq
        initSeq = (V.length xs, (xs V.!))
    do
      let fillLen = case ex of
            Nothing -> 272
            Just ~[raw] -> read raw
      answerS $ ppr $ solve fillLen initSeq
    do
      let fillLen = case ex of
            Nothing -> 35651584
            Just _ -> 0x70000
      answerS $ ppr $ solve fillLen initSeq
