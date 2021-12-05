{-# LANGUAGE TemplateHaskell #-}

module Javran.AdventOfCode.TestDataSpec where

import Javran.AdventOfCode.TestDataTh
import Test.Hspec

{-

  TODO: current approach has a problem that
  when testfiles are updates, this is not recompiled,
  see what can we do about it.

  probably we can do something stupid,
  like edit this file every time we write-expect.

 -}

spec :: Spec
spec = mkSpecFromStructuredTestData $collectTests
