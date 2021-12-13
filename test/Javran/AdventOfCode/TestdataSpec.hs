{-

This module runs TH, which detects changes in data/testdata and constructs corresponding
Spec tree accordingly.

Heavy lifting are done in Javran.AdventOfCode.TestdataTh
and this module is intentionally disable for diffing,
as I'd expect most of the changes going forward are uninteresting to human eyes.

 -}
{-# LANGUAGE TemplateHaskell #-}

module Javran.AdventOfCode.TestdataSpec
  ( spec
  , hashForForceRecompliation
  )
where

import Javran.AdventOfCode.TestdataTh
import Test.Hspec

{-
  The following definition is used to keep track of a digest of
  all filepaths within data/testdata [1], this is done
  to force the build system to recompile (thus re-run TH) this module again.

  We only expect the hash line to change should directory structure
  of data/testdata changes.

  1: the TH stuff doesn't care about file content,
    which is actually read by the unit tests.

 -}

{- ORMOLU_DISABLE -}
hashForForceRecompliation :: String
-- FORCE_RECOMP_HASH_BEGIN
hashForForceRecompliation = "9ef6b802d0e4a95b4e76c92aded18b0faf33c8bb31fe6767658bbebfbbed79e5"
-- FORCE_RECOMP_HASH_END
{- ORMOLU_ENABLE -}

spec :: Spec
spec = mkSpecFromStructuredTestdata $collectTests
