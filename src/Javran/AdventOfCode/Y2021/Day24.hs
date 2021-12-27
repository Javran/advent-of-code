{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Javran.AdventOfCode.Y2021.Day24
  ( Alu
  , runMystery
  , mystery
  )
where

import Control.Lens hiding (op)
import Control.Monad
import Control.Monad.State.Strict
import Data.List.Split hiding (sepBy)
import Javran.AdventOfCode.Prelude
import qualified Javran.AdventOfCode.Y2021.Day24.Z3 as Z3
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day24 deriving (Generic)

data Reg = RegW | RegX | RegY | RegZ
  deriving (Eq, Ord, Enum, Bounded, Show)

data ReadOnly = RoReg Reg | RoLit Int
  deriving (Show, Eq)

data Instr
  = Inp Reg
  | Add Reg ReadOnly
  | Mul Reg ReadOnly
  | Div Reg ReadOnly
  | Mod Reg ReadOnly
  | Eql Reg ReadOnly
  deriving (Show, Eq)

instrP :: ReadP Instr
instrP =
  (string "inp " *> (Inp <$> regP)) <++ do
    let opName ~> op = do
          _ <- op <$ string opName
          _ <- char ' '
          lhs <- regP
          _ <- char ' '
          rhs <- (RoReg <$> regP) <++ (RoLit <$> readS_to_P (reads @Int))
          pure $ op lhs rhs
    foldl1
      (<++)
      [ "add" ~> Add
      , "mul" ~> Mul
      , "div" ~> Div
      , "mod" ~> Mod
      , "eql" ~> Eql
      ]
  where
    regP =
      let ch ~> v = v <$ char ch
       in foldl1
            (<++)
            [ 'w' ~> RegW
            , 'x' ~> RegX
            , 'y' ~> RegY
            , 'z' ~> RegZ
            ]

type Alu = (Int, Int, Int, Int)

runInstr :: Instr -> State (Alu, [Int]) ()
runInstr = \case
  Inp reg -> do
    inp <- state (\(alu, x : xs) -> (x, (alu, xs)))
    modify (first (_reg reg .~ inp))
  Add l r ->
    liftOp l r (+)
  Mul l r ->
    liftOp l r (*)
  Div l r ->
    liftOp l r (\a b -> if b == 0 then error "crash (div)" else quot a b)
  Mod l r ->
    liftOp l r (\a b -> if a < 0 || b <= 0 then error "crash (mod)" else rem a b)
  Eql l r ->
    liftOp l r (\a b -> bool 0 1 (a == b))
  where
    liftOp regL roR op = do
      valL <- gets ((^. _reg regL) . fst)
      valR <- case roR of
        RoLit v -> pure v
        RoReg regR -> gets ((^. _reg regR) . fst)
      modify (first (_reg regL .~ op valL valR))

    _reg = \case
      RegW -> _1
      RegX -> _2
      RegY -> _3
      RegZ -> _4

runMystery :: Alu -> (Int, Int, Int) -> Int -> Alu
runMystery alu (a, b, c) inp = alu'
  where
    instrs = MysterySection a b c
    (alu', _) = execState (mapM runInstr instrs) (alu, [inp])

{-
  The pattern is easy to spot that we eventually ended up with
  this pattern matching to confirm that we only have 3 parameters to
  worry about.
 -}
pattern MysterySection :: Int -> Int -> Int -> [Instr]
pattern MysterySection a b c =
  [ Inp RegW
    , Mul RegX (RoLit 0)
    , Add RegX (RoReg RegZ)
    , Mod RegX (RoLit 26)
    , Div RegZ (RoLit a)
    , Add RegX (RoLit b)
    , Eql RegX (RoReg RegW)
    , Eql RegX (RoLit 0)
    , Mul RegY (RoLit 0)
    , Add RegY (RoLit 25)
    , Mul RegY (RoReg RegX)
    , Add RegY (RoLit 1)
    , Mul RegZ (RoReg RegY)
    , Mul RegY (RoLit 0)
    , Add RegY (RoReg RegW)
    , Add RegY (RoLit c)
    , Mul RegY (RoReg RegX)
    , Add RegZ (RoReg RegY)
    ]

matchChunk :: [Instr] -> (Int, Int, Int)
matchChunk xs = case xs of
  MysterySection a b c
    | (a == 1 && b >= 10) || (a == 26 && b <= 0)
      , c > 0 ->
      (a, b, c)
  _ -> error $ "cannot recognize: " <> show xs

{-
  Try to mimic what this mystery section does,
  QuickCheck to confirm that we have the correct impl.

  There are some interesting patterns from the dataset I'm looking at:

  - a can only be 1 or 26
  - a == 1 ==> b >= 10
  - a == 26 ==> b <= 0
  - 1 < c < 15

  So, probably this is what we can do:
  - if a == 1, there's no way that we can get z-value down,
    probably just exhaust the search space.
  - if a == 26, we *must* get some very specific w-value
    so that `z % 26 + b == w`

  Since there are 7 `a == 1` and 7 `a = 26` cases, we
  can hopefully get have 9 ^ 7 == 4782969 cases to test,
  which is probably not much for the computer.

 -}
mystery :: Int -> (Int, Int, Int) -> Int -> Int
mystery z (a, b, c) w = case a of
  1 ->
    {-
      Note that for a == 1, if we want to get a w value
      that we can go to the branch that doesn't cause z to increase:

      1 <= w <= 9
      => 1 <= z % 26 + b <= 9
      => 1 - b <= z % 26 <= 9 - b

      since b >= 10, 9 - b <= -1.
      there's no way for z % 26 to match this range,
      so for a == 1, we can cut it to just one branch.

      probably what we can do best here is to make it so that

      w + c < 26 (note that c > 0 and 1 <= w <= 9)
      (not helpful though, as it seems to be the case that c < 15)

     -}
    z * 26 + w + c
  26 ->
    let (q, r) = z `quotRem` 26
     in if r + b == w
          then q
          else q * 26 + w + c
  _ -> errInvalid

consider :: (Int, Int, Int) -> StateT (Int, [Int]) [] ()
consider p@(a, b, _c) = case a of
  1 -> do
    z <- gets fst
    w <- lift [1 .. 9]
    let z' = mystery z p w
    modify (bimap (const z') (w :))
  26 -> do
    z <- gets fst
    let w = z `rem` 26 + b
    guard $ w >= 1 && w <= 9
    let z' = mystery z p w
    modify (bimap (const z') (w :))
  _ -> errInvalid

solve :: [(Int, Int, Int)] -> [Int]
solve zs =
  evalStateT
    (do
       mapM_ consider zs
       z <- gets fst
       guard $ z == 0
       digitsToInt @Int . reverse <$> gets snd)
    (0, [])

instance Solution Day24 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (consumeOrDie instrP) . lines <$> getInputS
    let [] : ys = splitOn [Inp RegW] xs
        zs = fmap (matchChunk . (Inp RegW :)) ys
        ans = solve zs
        useZ3 = False
    if useZ3
      then do
        (part1, part2) <- Z3.solve zs
        answerShow part1
        answerShow part2
      else do
        answerShow (last ans)
        answerShow (head ans)

