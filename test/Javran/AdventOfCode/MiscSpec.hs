{-# LANGUAGE BlockArguments #-}

module Javran.AdventOfCode.MiscSpec
  ( spec
  )
where

import Data.List
import Javran.AdventOfCode.Misc
import Test.Hspec

spec :: Spec
spec = do
  describe "rotateLeftBy" $
    specify "example" $
      fmap (\i -> rotateLeftBy 5 i "ABCDE") [0 .. 5]
        `shouldBe` [ "ABCDE"
                   , "BCDEA"
                   , "CDEAB"
                   , "DEABC"
                   , "EABCD"
                   , "ABCDE"
                   ]
  describe "rotateRightBy" $
    specify "example" $
      fmap (\i -> rotateRightBy 5 i "abcde") [0 .. 5]
        `shouldBe` [ "abcde"
                   , "eabcd"
                   , "deabc"
                   , "cdeab"
                   , "bcdea"
                   , "abcde"
                   ]
  describe "nthPermutation" $ do
    let mkTest len = specify ("n = " <> show len) do
          let inp = take len ['a' ..]
              permCount = product [1 .. len]
              outs = fmap (\w -> nthPermutation len w inp) [0 .. permCount - 1]
          outs `shouldBe` sort (permutations inp)
    mapM_ mkTest [0 .. 6]
