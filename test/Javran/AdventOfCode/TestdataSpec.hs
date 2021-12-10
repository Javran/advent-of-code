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
c5f8af9462ecd9d3a8af1b72e5f16db702b30619f2f1ca766bbd85dab37825f9
FORCE_RECOMP_HASH_END

 -}
{-# LANGUAGE TemplateHaskell #-}

module Javran.AdventOfCode.TestdataSpec where

import Javran.AdventOfCode.TestdataTh
import Test.Hspec

spec :: Spec
spec = mkSpecFromStructuredTestdata $collectTests
