{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2019.Day9
  (
  )
where

import Data.List
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2019.IntCode

data Day9 deriving (Generic)

instance Solution Day9 where
  solutionRun _ SolutionContext {getInputS, answerS} = do
    (extraOps, rawInput) <- consumeExtraLeadingLines <$> getInputS
    let xs = parseCodeOrDie rawInput
    case extraOps of
      Just _ -> do
        -- running tests
        (_, outputs) <- runProgram xs []
        answerS $ intercalate "," (fmap show outputs)
      Nothing -> do
        do
          (_, outputs) <- runProgram xs [1]
          answerS $ intercalate "," (fmap show outputs)
        do
          (_, outputs) <- runProgram xs [2]
          answerS $ intercalate "," (fmap show outputs)
