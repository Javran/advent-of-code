{-# LANGUAGE BlockArguments #-}

module Javran.AdventOfCode.Y2021.Day24Spec
  ( spec
  )
where

import Control.Monad
import Javran.AdventOfCode.Y2021.Day24
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

genAlu :: Gen Alu
genAlu = do
  w <- chooseInt (1,9)
  xs <- replicateM 3 $ chooseInt (0, 9999)
  let [x, y, z] = xs
  pure (w, x, y, z)

genAbc :: Gen (Int, Int, Int)
genAbc = do
  a <- oneof [pure 1, pure 26]
  b <- chooseInt (-15, 15)
  c <- chooseInt (1, 15)
  pure (a, b, c)

spec :: Spec
spec =
  describe "Y2021" $
    describe "Day24" $
      prop "mystery === runMystery" $
        forAll
          ((,) <$> genAlu <*> genAbc)
          (\(alu@(w, _x, _y, z), abc) -> do
             let (_, _, _, expect) = runMystery alu abc w
                 actual = mystery z abc w
             expect === actual)
