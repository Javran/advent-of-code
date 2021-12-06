{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Javran.AdventOfCode.Cli.ProgressReport
  ( ProgressReport
  , progressReportCommand
  , getSolution
  )
where

{-
  Note that we can't rely on Javran.AdventOfCode.Y____.Main module
  since we might have just created some new solution and don't even
  have chance to rebuild yet.

  So instead we do the following:

  - Scan src/ to find solution files
  - to determine solution status: if we have a corresponding
    SomeSolution, use it. Otherwise that module isn't even rebuilt,
    meaning it is most likely unsolved.

 -}

import qualified Control.Foldl as Foldl
import qualified Data.IntMap.Strict as IM
import qualified Data.List.Ordered as LOrdered
import qualified Data.Map.Strict as M
import Data.Ord
import qualified Filesystem.Path.CurrentOS as FP
import Javran.AdventOfCode.Infra
import qualified Javran.AdventOfCode.Y2020.Main as Y2020
import Control.Monad
import qualified Javran.AdventOfCode.Y2021.Main as Y2021
import Text.ParserCombinators.ReadP
import Turtle.Prelude
import Turtle.Shell

getSolution :: Int -> Int -> Maybe SomeSolution
getSolution year day = allSolutions M.!? (year, day)
  where
    allSolutions = M.fromList $ do
      someSol@(SomeSolution s) <- Y2020.allSolutions <> Y2021.allSolutions
      let sInd = solutionIndex s
      pure (sInd, someSol)

type ProgressReport =
  [ ( Int -- year, descending.
    , [ ( Int -- day, ascending.
        , Bool -- solved?
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
    let solved = case getSolution year day of
          Nothing -> False
          Just (SomeSolution s) -> solutionSolved s
    pure (year, (day, solved))
  pure $
    IM.toDescList $
      IM.fromListWith (LOrdered.unionBy (comparing fst)) do
        (year, (day, solved)) <- gathered
        pure (year, [(day, solved)])

progressReportCommand :: String -> IO ()
progressReportCommand _ = do
  xs <- computeProgressReport
  forM_ xs $ \(year, days) -> do
    putStrLn $ "Year " <> show year <> ":"
    forM_ days $ \(day, solved) -> do
      putStrLn $ "- Day " <> show day <> if solved then "" else " (unsolved)"
