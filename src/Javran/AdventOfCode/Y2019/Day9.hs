{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2019.Day9
  (
  )
where

import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Vector.Unboxed as VU
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2019.IntCode

data Day9 deriving (Generic)

instance Solution Day9 where
  solutionRun _ SolutionContext {getInputS, answerS} = do
    (extraOps, rawInput) <- consumeExtraLeadingLines <$> getInputS
    let xs = fmap (read @Int) . splitOn "," . head . lines $ rawInput
        mem = VU.fromList xs
    case extraOps of
      Just _ -> do
        -- running tests
        (_, outputs) <- runProgram mem []
        answerS $ intercalate "," (fmap show outputs)
      Nothing -> do
        do
          (_, outputs) <- runProgram mem [1]
          answerS $ intercalate "," (fmap show outputs)
        do
          (_, outputs) <- runProgram mem [2]
          answerS $ intercalate "," (fmap show outputs)
