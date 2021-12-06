{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2020.Day25
  (
  )
where

import Control.Monad
import qualified Data.HashMap.Strict as HM
import Data.Mod
import GHC.Generics (Generic)
import GHC.Natural
import GHC.TypeNats (KnownNat, natVal)
import Javran.AdventOfCode.Prelude

data Day25 deriving (Generic)

type ModP = Mod 20201227

babyStepGiantStep :: KnownNat n => Mod n -> Mod n -> Maybe Int
babyStepGiantStep alpha beta = giantStep beta 0
  where
    rootMod = natVal alpha
    {-
      For computing square root on Int,
      it seems that we can do no better than a trip to floating points:
      https://github.com/Bodigrim/integer-roots/blob/7c1bff706149baf071cc41f6415da2a27b492079/Math/NumberTheory/Roots/Squares.hs#L210-L215
    -}
    m :: Int
    m = ceiling (sqrt (fromIntegral rootMod :: Double))
    babyStep :: HM.HashMap Natural Int
    babyStep = HM.fromList do
      j <- [0 .. m -1]
      pure (unMod (alpha ^% j), j)
    alphaPowNegM = alpha ^% (- m)
    giantStep gamma i = do
      guard $ i < m
      case HM.lookup (unMod gamma) babyStep of
        Nothing -> giantStep (gamma * alphaPowNegM) (i + 1)
        Just j -> pure (i * m + j)

-- https://en.wikipedia.org/wiki/Baby-step_giant-step#The_algorithm
instance Solution Day25 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    cPk : dPk : _ <- fmap (fromIntegral @_ @ModP . read @Int) . lines <$> getInputS
    let subject :: ModP
        subject = 7
        Just cSk = babyStepGiantStep subject cPk
    answerShow $ unMod $ dPk ^% cSk
