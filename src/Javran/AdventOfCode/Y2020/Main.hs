{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}

module Javran.AdventOfCode.Y2020.Main
  ( subMain
  )
where

import Data.List
import Data.Proxy
import qualified Data.Text.IO as T
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.TH
import Javran.AdventOfCode.Y2020.Day1 ()
import Javran.AdventOfCode.Y2020.Day2 ()
import Javran.AdventOfCode.Y2020.Day3 ()
import Javran.AdventOfCode.Y2020.Day4 ()

data SomeSolution = forall sol. Solution sol => SomeSolution (Proxy sol)

collectedSolutions :: [(Int, SomeSolution)]
collectedSolutions = sortOn fst (mk <$> $collectSolutions)
  where
    mk ss@(SomeSolution s) = case solutionIndex s of
      (2020, dd) -> (dd, ss)
      solInd -> error $ "Invalid solution index: " <> show solInd

runSomeSolution :: SomeSolution -> IO ()
runSomeSolution (SomeSolution s) = runSolutionWithLoginInput s >>= T.putStr

subMain :: String -> IO ()
subMain cmdHelpPrefix = do
  let solutionRunners =
        fmap
          (\(i, ss) -> (show i, const (runSomeSolution ss)))
          collectedSolutions
  dispatchToSubCmds
    cmdHelpPrefix
    solutionRunners
