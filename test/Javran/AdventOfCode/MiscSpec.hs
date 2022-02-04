module Javran.AdventOfCode.MiscSpec
  ( spec
  )
where

import Javran.AdventOfCode.Misc (rotateLeftBy, rotateRightBy)
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
