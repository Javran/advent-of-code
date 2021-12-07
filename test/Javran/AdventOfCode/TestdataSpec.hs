{-

This module runs TH, which detects changes in data/testdata and constructs corresponding
Spec tree accordingly.

Heavy lifting are done in Javran.AdventOfCode.TestdataTh
and this module is intentionally disable for diffing,
as I'd expect most of the changes going forward are uninteresting to human eyes.

We only expect the hash line to change should directory structure of data/testdata changes.

The section below is used to keep track of a digest of
all filepaths within data/testdata [1], this is done
to force the build system to recompile (thus re-run TH) this module again.

1: the TH stuff doesn't care about file content,
  which is actually read by the unit tests.

FORCE_RECOMP_HASH_BEGIN
9df98fe756844094d8ef4fc743357294e70403e88e0b8cdcab1d9ade7d41da42
FORCE_RECOMP_HASH_END

 -}
{-# LANGUAGE TemplateHaskell #-}

module Javran.AdventOfCode.TestdataSpec where

import Javran.AdventOfCode.TestdataTh
import Test.Hspec

spec :: Spec
spec = mkSpecFromStructuredTestdata $collectTests
