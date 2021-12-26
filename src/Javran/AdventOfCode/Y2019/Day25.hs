{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2019.Day25
  (
  )
where

import Control.Arrow
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2019.Day25.Common
import Javran.AdventOfCode.Y2019.Day25.Cracker
import Javran.AdventOfCode.Y2019.Day25.Explorer
import Javran.AdventOfCode.Y2019.IntCode

data Day25 deriving (Generic)

instance Solution Day25 where
  solutionRun _ SolutionContext {getInputS, answerShow} =
    getInputS
      >>= (parseCodeOrDie
             >>> startProgramFromFoldable
             >>> asciiRun
             >>> runExplorer)
      >>= runCracker
      >>= answerShow
