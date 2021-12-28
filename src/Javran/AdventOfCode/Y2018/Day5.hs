{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2018.Day5
  (
  )
where

import Data.Bits
import Data.Char
import qualified Data.Set as S
import Javran.AdventOfCode.Prelude

data Day5 deriving (Generic)

canReact :: Char -> Char -> Bool
canReact x y = toLower x == toLower y && (isUpper x `xor` isUpper y)

doReduction :: String -> String -> String
doReduction stk = \case
  [] -> reverse stk
  x : xs -> case stk of
    s : ss ->
      if canReact x s
        then doReduction ss xs
        else doReduction (x : stk) xs
    [] -> doReduction [x] xs

instance Solution Day5 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    [xs] <- lines <$> getInputS
    answerShow (length (doReduction [] xs))
    let unitTypes = S.fromList $ fmap toLower xs
    answerShow $
      minimum $ do
        c <- S.toList unitTypes
        pure $ length $ doReduction [] (filter ((/= c) . toLower) xs)
