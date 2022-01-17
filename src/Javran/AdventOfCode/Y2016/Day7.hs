{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2016.Day7
  (
  )
where

import Control.Monad
import Data.List.Split hiding (sepBy)
import qualified Data.Set as S
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day7 deriving (Generic)

{-
  Left: chunk inside square brackets
  Right: chunk outside square brackets
 -}
type Addr = [Either String String]

addrP :: ReadP Addr
addrP = many1 (outsideP <++ insideP)
  where
    outsideP = Right <$> munch1 (/= '[')
    insideP = Left <$> between (char '[') (char ']') (munch1 (/= ']'))

getAbbas :: String -> [(Char, Char)]
getAbbas = mapMaybe f . divvy 4 1
  where
    f ~[a, b, b', a'] = (a, b) <$ guard (a == a' && b == b' && a /= b)

getAbas :: String -> [(Char, Char)]
getAbas = mapMaybe f . divvy 3 1
  where
    f ~[a, b, a'] = (a, b) <$ guard (a == a' && b /= a)

getBabs :: String -> [(Char, Char)]
getBabs = mapMaybe f . divvy 3 1
  where
    f ~[b, a, b'] = (a, b) <$ guard (b == b' && b /= a)

features :: Addr -> (Bool, Bool)
features a = (supportsTls, supportsSsl)
  where
    supportsTls = null (concatMap getAbbas insides) && not (null (concatMap getAbbas outsides))
    supportsSsl = not (S.null $ S.intersection abas babs)
    abas = S.fromList $ concatMap getAbas outsides
    babs = S.fromList $ concatMap getBabs insides

    (insides, outsides) = partitionEithers a

instance Solution Day7 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (consumeOrDie addrP) . lines <$> getInputS
    let supports = fmap features xs
    answerShow $ countLength fst supports
    answerShow $ countLength snd supports
