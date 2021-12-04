{-# LANGUAGE TemplateHaskell #-}

module Javran.AdventOfCode.TestDataSpec where

import Javran.AdventOfCode.TestDataTh
import Test.Hspec

spec :: Spec
spec = mkSpecFromStructuredTestData $collectTests
