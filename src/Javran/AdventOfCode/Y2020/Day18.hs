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

exprP :: ReadP Expr
exprP =
  makeExprParser
    termP
    [ [ binary "+" (EBin Plus)
      , binary "*" (EBin Mult)
      ]
    ]
  where
    termP =
      (tok (char '(') *> exprP <* tok (char ')'))
        <++ (ENum <$> tok decimal1P)

expr2P :: ReadP Expr
expr2P =
  makeExprParser
    termP
    [ [binary "+" (EBin Plus)]
    , [binary "*" (EBin Mult)]
    ]
  where
    termP =
      (tok (char '(') *> expr2P <* tok (char ')'))
        <++ (ENum <$> tok decimal1P)

binary :: String -> (Expr -> Expr -> Expr) -> Operator ReadP Expr
binary sym f = InfixL (f <$ tok (string sym))

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
      let xs = fmap (fromJust . consumeAllWithReadP exprP) rawLines
      answerShow (sum $ fmap eval xs)
    do
      let xs = fmap (fromJust . consumeAllWithReadP expr2P) rawLines
      answerShow (sum $ fmap eval xs)
