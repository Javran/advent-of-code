{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Javran.AdventOfCode.Y2021.Day10
  (
  )
where

import Data.Either
import Data.List
import Data.Monoid
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude

data Day10 deriving (Generic)

type Stack = [] Char

type SyntaxError =
  Either
    Stack -- left: partial stack
    Char -- right: encountered illegal char

checkSyntax :: Stack -> String -> Either SyntaxError ()
checkSyntax stk = \case
  [] -> if null stk then Right () else Left (Left stk)
  y : ys ->
    let push ch = checkSyntax (ch : stk) ys
     in case y of
          '(' -> push ')'
          '[' -> push ']'
          '{' -> push '}'
          '<' -> push '>'
          c -> case stk of
            [] -> Left (Right c)
            s : stk' ->
              if c == s
                then checkSyntax stk' ys
                else Left (Right c)

instance Solution Day10 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- lines <$> getInputS
    let (syntaxErrors, _corrects) =
          partitionEithers (fmap (checkSyntax []) xs)
        (partials, illegals) = partitionEithers syntaxErrors
    do
      let toScore = \case
            ')' -> 3 :: Sum Int
            ']' -> 57
            '}' -> 1197
            '>' -> 25137
            _ -> 0
      answerShow $ getSum $ foldMap toScore illegals
    do
      let toScore = foldl (\acc i -> acc * 5 + tr i) 0
            where
              tr = \case
                ')' -> 1 :: Int
                ']' -> 2
                '}' -> 3
                '>' -> 4
                _ -> 0
          ys = sort $ fmap toScore partials
          l = length ys
      answerShow (ys !! (l `quot` 2))
