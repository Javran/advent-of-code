{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Javran.AdventOfCode.Cli.SolutionCommand
  ( parse
  , CommandMode (..)
  , subCommand
  )
where

{-
  This module handles all solution-specific commands,
  rather than having each year as a seperated command.

  Accepted commands:

  - "ls" for listing.
  - any integer value (<year>) commits to this subcommand.
  - <year>: list avaliable day solutions for that year
  - <year> <day> <subcommand> <subcommand args>...: this should preserve
    the old behavior.
  - <year> <day> new: create solution from template

  (TODO) Example tooling:

  - <prog> <year> <day> edit-example [<arg>]

    + assume "example" if <arg> is not given
    + reserve "all" to run all examples (not allowed to edit)
    + if "+", iterate from 0 until one empty file (or non-existing file), and edit it.
    + otherwise read from <arg>.input.txt

  - <prog> <year> <day> example [<arg>]

    for determining the input file:

    + assume "example" if <arg> is not given
    + reserve "all" to run all examples
    + otherwise read from <arg>.input.txt

 -}

import qualified Data.IntMap.Strict as IM
import Javran.AdventOfCode.Cli.SolutionCommand.SolutionMain
import Javran.AdventOfCode.Infra
import Javran.AdventOfCode.Solutions

data CommandMode
  = ModeList [String]
  | ModeYear Int [String]
  | ModeYearDay Int Int [String]

parse :: [String] -> Maybe CommandMode
parse = \case
  "ls" : xs -> pure $ ModeList xs
  yearRaw : xs | Just year <- consumeAllWithReadP decimal1P yearRaw ->
    case xs of
      dayRaw : ys
        | Just day <- consumeAllWithReadP decimal1P dayRaw ->
          pure $ ModeYearDay year day ys
      ys -> pure $ ModeYear year ys
  _ -> Nothing

{-

  Handles commands dispatched from main entry,
  in many cases this is further dispatched to SolutionMain.runMainWith
  if a specific year and day are given.

 -}
subCommand :: SubCmdContext -> CommandMode -> IO ()
subCommand ctxt mode = do
  let SubCmdContext {cmdHelpPrefix} = ctxt
  case mode of
    ModeList _ ->
      {-
        the difference between this and `report` command is that this one only sees
        through TH generation and never explore the file system.
       -}
      mapM_ (print . (\(SomeSolution s) -> solutionIndex s)) allSolutionsSorted
    ModeYear year _ ->
      case allSolutions IM.!? year of
        Nothing -> do
          putStrLn "No solution available, valid years:"
          print (IM.keys allSolutions)
        Just yearSols ->
          mapM_ (print . (\(_, SomeSolution s) -> solutionIndex s)) (IM.toAscList yearSols)
    ModeYearDay year day xs ->
      runMainWith
        ctxt
          { cmdHelpPrefix =
              cmdHelpPrefix
                <> show year
                <> " "
                <> show day
                <> " "
          }
        year
        day
        xs
