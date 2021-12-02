module Javran.AdventOfCode.Main
  ( main
  )
where

import Javran.AdventOfCode.Infra
import qualified Javran.AdventOfCode.Y2020.Main as Y2020
import qualified Javran.AdventOfCode.Y2021.Main as Y2021

main :: IO ()
main =
  dispatchToSubCmds
    "<prog> "
    [ ("2020", Y2020.subMain)
    , ("2021", Y2021.subMain)
    ]
