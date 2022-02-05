{-# LANGUAGE BlockArguments #-}

module Javran.AdventOfCode.Y2016.Day21Spec
  ( spec
  )
where

import Control.Monad
import Data.List
import Javran.AdventOfCode.Misc (nthPermutation)
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2016.Day21
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

{-
  Generates permutation of n elements, whose elements are ['a' ..]
 -}
genStr :: Int -> Gen String
genStr n = do
  which <- chooseInt (0, product [0 .. n] - 1)
  pure $ nthPermutation n which $ take n ['a' ..]

genOperation :: Int -> Gen Operation
genOperation n =
  oneof
    [ mkBin nums SwapPos
    , mkBin letters SwapCh
    , do
        mk <- oneof [pure Left, pure Right]
        i <- pickOne nums
        pure $ RotStep $ mk i
    , do
        ch <- pickOne letters
        pure $ RotCh ch
    , mkBin nums $ curry \p ->
        let MinMax (i, j) = minMaxFromPair p
         in Rev i j
    , mkBin nums Move
    ]
  where
    nums = [0 .. n -1]
    letters = take n ['a' ..]
    mkBin xs f = do
      (a, b) <- pickTwo xs
      pure $ f a b

    pickOne xs = do
      i <- chooseInt (0, n -1)
      pure $ xs !! i
    pickTwo xs = do
      i <- chooseInt (0, n -1)
      let (a, xs') = pick xs !! i
      j <- chooseInt (0, n -2)
      pure (a, xs' !! j)

spec :: Spec
spec =
  describe "Y2016" do
    describe "Day21" do
      prop "unapplyOp then applyOp is consistent" $ forAll
        (do
           n <- chooseInt (5, 10)
           result <- genStr n
           opCount <- chooseInt (1, 10)
           ops <- replicateM opCount (genOperation n)
           pure (n, result, ops))
        \(n, result, ops) -> do
          {-
            If we follow a sequence of operations `ops` to arrive at `result`,
            then undoing those operations in reverse should give us `input`.
            Since some of the operations are destructive, we expect potentially
            many of them, and as long as all those inputs can be reapplied
            to get `result`, we are fine.

            TODO: investigate why there are zero-alternative cases.
            (probably has something to do with the destructive nature?)
           -}
          let inputs = foldM (\cur op -> unapplyOp n op cur) result (reverse ops)
              tag = "alternative count: " <> show (length inputs)
          label tag $
            conjoin $ forM_ inputs \inp -> do
              let applied = foldl' (\cur op -> applyOp n op cur) inp ops
              pure $ applied === result
