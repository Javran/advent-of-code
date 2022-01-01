{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Javran.AdventOfCode.Cli.ProgressReport
  ( ProgressReport
  , progressReportCommand
  , performReadmeProgressSync
  , computeProgressReport
  , renderRawMarkdown
  )
where

{-
  It's tempting that we import all solutions from Javran.AdventOfCode.Solutions,
  and use the information there for solving status.

  However it might be the case that the binary is called before rebuilding,
  in which case the module won't even exist within the binary.

  So instead we do the following:

  - Scan src/ to find solution files
  - to determine solution status: if we can find the Solution, use the info there,
    otherwise assume unsolved.

 -}

import qualified Control.Foldl as Foldl
import Control.Monad
import Control.Monad.Writer.CPS
import qualified Data.IntMap.Strict as IM
import Data.List
import qualified Data.List.NonEmpty as NE
import qualified Data.List.Ordered as LOrdered
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as M
import Data.Ord
import qualified Filesystem.Path.CurrentOS as FP
import Javran.AdventOfCode.Infra
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Solutions
import System.Environment
import System.FilePath.Posix
import Text.ParserCombinators.ReadP
import Text.Printf
import Turtle.Prelude hiding (nl)
import Turtle.Shell

type ProgressReport =
  [ ( Int -- year, descending.
    , [ ( Int -- day, ascending.
        , Bool -- solved?
        )
      ]
    )
  ]

type ProgressReportTabulated =
  [ ( Int
    , NE.NonEmpty [Maybe (Int, Bool)]
    )
  ]

tabulateYear :: [(Int, Bool)] -> Maybe (NE.NonEmpty [Maybe (Int, Bool)])
tabulateYear xs = do
  (MinMax rng@(xMin, xMax)) <- foldMap (Just . minMax . fst) xs
  when (xMin < 1 || xMax > 25) do
    error $ "cannot tabulate: unexpected range: " <> show rng
  let m = M.fromList xs
      getCell i = do
        v <- m M.!? i
        pure (i, v)
  NE.nonEmpty $
    filter (any isJust) $
      (fmap . fmap) getCell $ chunksOf 5 [1 .. 25]

tabulateReport :: ProgressReport -> ProgressReportTabulated
tabulateReport = mapMaybe ((\(i, m) -> (i,) <$> m) . second tabulateYear)

computeProgressReport :: IO ProgressReport
computeProgressReport = do
  Just prjHomePre <- need "PROJECT_HOME"
  let prjHome = FP.fromText prjHomePre
  gathered <- reduce Foldl.list do
    pushd (prjHome FP.</> "src" FP.</> "Javran" FP.</> "AdventOfCode")
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
        (year, p) <- gathered
        pure (year, [p])

{-

Renders something like:

- Year 2020
  + [X] [Day 1](src/Javran/AdventOfCode/Y2020/Day1.hs)
  + [ ] [Day 2](src/Javran/AdventOfCode/Y2020/Day2.hs)

 -}

renderRawMarkdown :: ProgressReport -> [String]
renderRawMarkdown = concatMap (uncurry renderYear)
  where
    renderYear year days = ("- Year " <> show year) : fmap (uncurry renderDay) days
      where
        renderDay :: Int -> Bool -> String
        renderDay day solved =
          printf
            "  + [%c] [Day %d](src/Javran/AdventOfCode/Y%d/Day%d.hs)"
            (if solved then 'X' else ' ')
            day
            year
            day

renderTabulatedRawMarkdown :: ProgressReportTabulated -> [String]
renderTabulatedRawMarkdown prt = snd $ runWriter do
  let p x = tell [x]
      nl = p ""
      renderCell :: Int -> Maybe (Int, Bool) -> String
      renderCell year = \case
        Nothing -> ""
        Just (day, done) ->
          printf
            "%s [Day %d](src/Javran/AdventOfCode/Y%d/Day%d.hs)"
            (bool "☐" "☑" done :: String)
            day
            year
            day
  forM_ prt $ \(year, xs) -> do
    p $ "### " <> show year
    nl
    p $ intercalate " `<++++++++>` " $ replicate 6 "|"
    p $ intercalate " :-: " $ replicate 6 "|"
    forM_ xs \r -> do
      p $ "| " <> intercalate " | " (fmap (renderCell year) r) <> " |"
    nl

progressReportCommand :: SubCmdContext -> IO ()
progressReportCommand _ = do
  xs <- computeProgressReport
  forM_ xs $ \(year, days) -> do
    putStrLn $ "Year " <> show year <> ":"
    forM_ days $ \(day, solved) -> do
      putStrLn $ "- Day " <> show day <> if solved then "" else " (unsolved)"

performReadmeProgressSync :: IO ()
performReadmeProgressSync = do
  projectHome <- getEnv "PROJECT_HOME"
  let fp = projectHome </> "README.md"
  pr <- computeProgressReport
  let prt = tabulateReport pr
      rendered0 = renderTabulatedRawMarkdown prt
      rendered =
        -- need padding on both sides, or it would result in some weird renderings.
        "" : rendered0 <> [""]
      extractSecCb =
        (\prevSec bm sec em postSec ->
           if sec == rendered
             then -- no need for editing, nothing is changed.
               (sec, Just False)
             else
               ( prevSec <> [bm] <> rendered <> [em] <> postSec
               , Just True
               ))
  mayEditFileWithSpecialSection
    fp
    "README: "
    "[//]: # (PROGRESS_AUTOGEN_BEGIN)"
    "[//]: # (PROGRESS_AUTOGEN_END)"
    extractSecCb
