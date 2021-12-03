{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2020.Day18
  (
  )
where

{- HLINT ignore "Unused LANGUAGE pragma" -}

import Control.Monad
import Control.Monad.Combinators.Expr
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP

data Day18

data Op = Plus | Mult deriving (Show)

data Expr
  = ENum Int
  | EBin Expr Op Expr
  deriving (Show)

tok p = p <* skipSpaces

exprP :: ReadP Expr
exprP =
  makeExprParser
    termP
    [ [ binary "+" (\l r -> EBin l Plus r)
      , binary "*" (\l r -> EBin l Mult r)
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
    [ [binary "+" (\l r -> EBin l Plus r)]
    , [binary "*" (\l r -> EBin l Mult r)]
    ]
  where
    termP =
      (tok (char '(') *> expr2P <* tok (char ')'))
        <++ (ENum <$> tok decimal1P)

binary name f = InfixL (f <$ tok (string name))

eval :: Expr -> Int
eval = \case
  ENum v -> v
  EBin l op r ->
    let f = case op of
          Plus -> (+)
          Mult -> (*)
     in eval l `f` eval r

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
