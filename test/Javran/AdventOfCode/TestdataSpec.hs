{-# LANGUAGE TemplateHaskell #-}

module Javran.AdventOfCode.TestdataSpec where

import Javran.AdventOfCode.TestdataTh
import Test.Hspec

{-

The section below is used to keep track of a digest of
all filepaths within data/testdata [1], this is done
to force the build system to recompile (thus re-run TH) this module again.

1: the TH stuff doesn't care about file content,
  which is actually read by the unit tests.

FORCE_RECOMP_HASH_BEGIN
TODO: not implemented yet
FORCE_RECOMP_HASH_END

 -}

spec :: Spec
spec = mkSpecFromStructuredTestdata $collectTests
