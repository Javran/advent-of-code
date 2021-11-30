{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}

module Javran.AdventOfCode.Y2020.Main
  ( subMain
  )
where

import Data.Bifunctor
import Data.List
import Data.Proxy
import qualified Data.Text.IO as T
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.TH
import Javran.AdventOfCode.Y2020.Day1 ()
import Javran.AdventOfCode.Y2020.Day2 ()
import Javran.AdventOfCode.Y2020.Day3 ()
import Javran.AdventOfCode.Y2020.Day4 ()
import Javran.AdventOfCode.Y2020.Day5 ()
import Javran.AdventOfCode.Y2020.Day6 ()
import Javran.AdventOfCode.Y2020.Day7 ()
import Javran.AdventOfCode.Y2020.Day8 ()
import Javran.AdventOfCode.Y2020.Day9 ()
import Javran.AdventOfCode.Y2020.Day10 ()
import System.Environment
import System.Exit

data SomeSolution = forall sol. Solution sol => SomeSolution (Proxy sol)

collectedSolutions :: [(Int, SomeSolution)]
collectedSolutions = sortOn fst (mk <$> $collectSolutions)
  where
    mk ss@(SomeSolution s) = case solutionIndex s of
      (2020, dd) -> (dd, ss)
      solInd -> error $ "Invalid solution index: " <> show solInd

runSomeSolution :: SomeSolution -> String -> IO ()
runSomeSolution (SomeSolution s) cmdHelpPrefix = do
  args <- getArgs
  case args of
    [] ->
      runSolutionWithLoginInput s >>= T.putStr
    ["login"] ->
      runSolutionWithLoginInput s >>= T.putStr
    ["example"] ->
      runSolutionWithExampleInput s >>= T.putStr
    ["write-expect"] ->
      runSolutionWithExampleAndWriteExpect s
    _ ->
      die $ cmdHelpPrefix <> "[example|login|write-expect]"

subMain :: String -> IO ()
subMain cmdHelpPrefix = do
  let solutionRunners =
        fmap
          (bimap show runSomeSolution)
          collectedSolutions
  dispatchToSubCmds
    cmdHelpPrefix
    solutionRunners
