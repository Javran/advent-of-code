{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Javran.AdventOfCode.Cli.ProgressReport
  ( ProgressReport
  , progressReportCommand
  , computeProgressReport
  , renderRawMarkdown
  )
where

{-
  TODO: it's tempting that we import all solutions from Javran.AdventOfCode.Solutions,
  which will result in a cyclilar module dependency, understandably.

  So instead we do the following:

  - Scan src/ to find solution files
  - to determine solution status: we probably need to organize those modules in some other ways,
    this is for future me to figure out.

 -}

import qualified Control.Foldl as Foldl
import Control.Monad
import qualified Data.IntMap.Strict as IM
import qualified Data.List.Ordered as LOrdered
import Data.Ord
import qualified Filesystem.Path.CurrentOS as FP
import Javran.AdventOfCode.Infra
import Text.ParserCombinators.ReadP
import Text.Printf
import Turtle.Prelude
import Turtle.Shell

type ProgressReport =
  [ ( Int -- year, descending.
    , [ ( Int -- day, ascending.
        , Bool -- solved? (not used)
        )
      ]
    )
  ]

computeProgressReport :: IO ProgressReport
computeProgressReport = do
  Just prjHomePre <- need "PROJECT_HOME"
  let prjHome = FP.fromText prjHomePre
  gathered <- reduce Foldl.list do
    let (</>) = (FP.</>)
    pushd (prjHome </> "src" </> "Javran" </> "AdventOfCode")
    fp <- lstree "."
    [_dot, yearRaw, dayRaw] <- pure (FP.encodeString <$> FP.splitDirectories fp)
    Just year <- pure (consumeAllWithReadP (char 'Y' *> decimal1P <* char '/') yearRaw)
    Just day <- pure (consumeAllWithReadP (string "Day" *> decimal1P <* string ".hs") dayRaw)
    pure (year, (day, error "TODO: don't use this field."))
  pure $
    IM.toDescList $
      IM.fromListWith (LOrdered.unionBy (comparing fst)) do
        (year, p) <- gathered
        pure (year, [p])

{-

Renders something like:

- Year 2020
  + [Day 1](src/Javran/AdventOfCode/Y2020/Day1.hs)
  + [Day 2](src/Javran/AdventOfCode/Y2020/Day2.hs)

 -}

renderRawMarkdown :: ProgressReport -> [String]
renderRawMarkdown = concatMap (uncurry renderYear)
  where
    renderYear year days = ("- Year " <> show year) : fmap (uncurry renderDay) days
      where
        renderDay :: Int -> Bool -> String
        renderDay day _ =
          printf
            "  + [Day %d](src/Javran/AdventOfCode/Y%d/Day%d.hs)"
            day
            year
            day

progressReportCommand :: String -> IO ()
progressReportCommand _ = do
  xs <- computeProgressReport
  forM_ xs $ \(year, days) -> do
    putStrLn $ "Year " <> show year <> ":"
    forM_ days $ \(day, _) -> do
      putStrLn $ "- Day " <> show day
