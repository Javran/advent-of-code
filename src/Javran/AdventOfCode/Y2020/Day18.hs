{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2020.Day18
  (
  )
where

import Control.Monad.Combinators.Expr
import Data.Maybe
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP

data Day18

data Op = Plus | Mult deriving (Show)

data Expr
  = ENum Int
  | EBin Op Expr Expr
  deriving (Show)

tok :: ReadP a -> ReadP a
tok p = p <* skipSpaces

plusExprP, multExprP :: Operator ReadP Expr
( plusExprP
  , multExprP
  ) =
    ( binary "+" (EBin Plus)
    , binary "*" (EBin Mult)
    )
    where
      binary sym f = InfixL (f <$ tok (string sym))

makeParser :: [[Operator ReadP Expr]] -> ReadP Expr
makeParser precedRules = exprP
  where
    exprP =
      makeExprParser
        termP
        precedRules
    termP =
      (tok (char '(') *> exprP <* tok (char ')'))
        <++ (ENum <$> tok decimal1P)

parser :: ReadP Expr
parser =
  makeParser
    [ [ plusExprP
      , multExprP
      ]
    ]

parser2 :: ReadP Expr
parser2 =
  makeParser
    [ [plusExprP]
    , [ multExprP
      ]
    ]

eval :: Expr -> Int
eval = \case
  ENum v -> v
  EBin op l r ->
    (case op of
       Plus -> (+)
       Mult -> (*))
      (eval l)
      (eval r)

instance Solution Day18 where
  solutionIndex _ = (2020, 18)
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    rawLines <- lines <$> getInputS
    do
      let xs = fmap (fromJust . consumeAllWithReadP parser) rawLines
      answerShow (sum $ fmap eval xs)
    do
      let xs = fmap (fromJust . consumeAllWithReadP parser2) rawLines
      answerShow (sum $ fmap eval xs)
