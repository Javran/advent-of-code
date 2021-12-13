{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2019.Day5
  (
  )
where

import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2019.IntCode

data Day5 deriving (Generic)

instance Solution Day5 where
  solutionRun _ SolutionContext {getInputS, answerShow, answerS} = do
    (extraOps, rawInput) <- consumeExtraLeadingLines <$> getInputS
    let xs = parseCodeOrDie rawInput
    case extraOps of
      Nothing -> do
        -- running with login example
        do
          (_afterMem, logs) <- runProgram xs [1]
          answerS "Part 1:"
          mapM_ answerShow logs
        do
          (_afterMem, logs) <- runProgram xs [5]
          answerS "Part 2:"
          mapM_ answerShow logs
      Just rawTestInputs -> do
        -- running with testdata
        let inputs :: [Int]
            inputs = fmap read . filter ((/= "#") . take 1) $ rawTestInputs
        (_afterMem, logs) <- runProgram xs inputs
        mapM_ answerShow logs
