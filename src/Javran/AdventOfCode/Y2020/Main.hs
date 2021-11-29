module Javran.AdventOfCode.Y2020.Main
  ( subMain
  )
where

import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2020.Day1 as Day1
import Javran.AdventOfCode.Y2020.Day2 as Day2
import Javran.AdventOfCode.Y2020.Day3 as Day3

subMain :: String -> IO ()
subMain cmdHelpPrefix =
  dispatchToSubCmds
    cmdHelpPrefix
    [ ("1", const Day1.main)
    , ("2", const Day2.main)
    , ("3", const Day3.main)
    ]
