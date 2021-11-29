module Javran.AdventOfCode.Main
  ( main
  )
where

import Javran.AdventOfCode.Prelude
import qualified Javran.AdventOfCode.Y2020.Main as Y2020

main :: IO ()
main =
  dispatchToSubCmds
    "<prog> "
    [("2020", Y2020.subMain)]
