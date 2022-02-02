{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2016.Day21
  (
  )
where

import Control.Lens hiding (op)
import Control.Monad
import Data.List
import qualified Data.Map.Strict as M
import Javran.AdventOfCode.Misc
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.TestExtra
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day21 deriving (Generic)

data Operation
  = SwapPos Int Int
  | SwapCh Char Char
  | RotStep (Either Int Int)
  | RotCh Char
  | Rev Int Int
  | Move Int Int
  deriving (Show)

operationP :: ReadP Operation
operationP =
  (string "swap " *> swapsP)
    <++ (string "rotate " *> rotatesP)
    <++ revP
    <++ moveP
  where
    positionP = string "position " *> decimal1P
    letterP = string "letter " *> nextCharP
    swapsP =
      (do
         x <- positionP
         strP " with "
         y <- positionP
         pure $ SwapPos x y)
        <++ (do
               x <- letterP
               strP " with "
               y <- letterP
               pure $ SwapCh x y)
    rotatesP =
      (do
         strP "based on position of "
         x <- letterP
         pure $ RotCh x)
        <++ (do
               mk <- (Left <$ string "left") <++ (Right <$ string "right")
               charP ' '
               x <- decimal1P
               strP " step"
               _ <- void (char 's') <++ pure ()
               pure $ RotStep (mk x))
    revP = do
      strP "reverse positions "
      xIn <- decimal1P
      strP " through "
      yIn <- decimal1P
      let MinMax (x, y) = minMaxFromPair (xIn, yIn)
      pure $ Rev x y
    moveP = do
      strP "move "
      x <- positionP
      strP " to "
      y <- positionP
      pure $ Move x y

applyOp :: Int -> Operation -> [] Char -> [] Char
applyOp n = \case
  SwapPos i j -> \xs ->
    let a = xs !! i
        b = xs !! j
     in xs & ix i .~ b & ix j .~ a
  SwapCh a b -> \xs ->
    let Just i = elemIndex a xs
        Just j = elemIndex b xs
     in xs & ix i .~ b & ix j .~ a
  RotStep (Left i) -> rotateLeftBy n i
  RotStep (Right i) -> rotateRightBy n i
  RotCh x -> \xs ->
    let Just i = elemIndex x xs
        extra = if i >= 4 then 1 else 0
     in rotateRightBy n ((i + 1 + extra) `mod` n) xs
  Rev i j ->
    let sliceLen = j - i + 1
        f xs = reverse ys <> zs
          where
            (ys, zs) = splitAt sliceLen xs
     in rotateRightBy n i . f . rotateLeftBy n i
  Move i j -> \xs ->
    let x = xs !! i
     in fmap snd . insertBy (comparing fst) (j, x) . zip [0 ..] . delete x $ xs

{-
  Attempts to undo an operation.

  With runtime checks - we could certainly go faster, but I'd say
  the program is fast enough that we can afford having those checks in place.

  TODO: QuickCheck probably?
 -}
unapplyOp :: Int -> Operation -> [] Char -> [[] Char]
unapplyOp n = \case
  op@(RotCh x) -> \ys -> do
    {-
      n is small enough that we can afford trying them all
      TODO: probably something smarter than this?
     -}
    offset <- [0 .. n -1]
    let xs = rotateLeftBy n offset ys
        Just i = elemIndex x xs
        extra = if i >= 4 then 1 else 0
    guard $ offset == ((i + 1 + extra) `mod` n)
    if applyOp n op xs == ys
      then pure xs
      else error $ "violated: " <> show (xs, op, ys)
  op -> \ys ->
    let op' = case op of
          RotStep e -> RotStep $ either Right Left e
          Move i j -> Move j i
          _ -> op
        xs = applyOp n op' ys
     in if applyOp n op xs == ys
          then [xs]
          else error $ "violated: " <> show (op, op', xs, ys)

instance Solution Day21 where
  solutionRun _ SolutionContext {getInputS, answerS} = do
    (ex, rawInput) <- consumeExtra getInputS
    let ops = fmap (consumeOrDie operationP) . lines $ rawInput
        initSeq = case ex of
          Nothing -> "abcdefgh"
          Just ~[s] -> s
        applyAll z = foldl' (\cur op -> applyOp (length initSeq) op cur) z ops
        xs = applyAll initSeq
        genExample = False
    {-
      While attempting to create some testcases, I noticed that
      there are cases where two different input sequence results
      in the same sequence, suggesting that some of those operations are
      probably irreversible.

      Despite that, we can still run operations backwards,
      just that we need to return multiple alternatives.
     -}
    when genExample do
      let uniqueInputs = M.filter ((== 1) . length) $ M.fromListWith (<>) do
            p <- permutations initSeq
            let result = applyAll p
            pure (result, [p])
      print uniqueInputs

    case ex of
      Nothing -> do
        answerS xs
        do
          let p2Target = "fbgdceah"
              l = length p2Target
              inputs = foldr (\op cur -> cur >>= unapplyOp l op) [p2Target] ops
          answerS $ head inputs
      Just _ -> do
        mapM_ answerS $
          tail $ scanl' (\cur op -> applyOp (length initSeq) op cur) initSeq ops
