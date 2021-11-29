
module DemoSpec where

import Test.Hspec

spec :: Spec
spec =
  describe "demo" $
    specify "trivial" $
      () `shouldBe` ()
