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
  note that any integer value (<year>) commits to this subcommand.
  (in other words, running `<prog> <int> ...` will not go back to the main dispatching logic).

  Accepted commands:

  - "ls" for listing.
  - <year>: list avaliable day solutions for that year.
  - <year> ls: same as above.
  - <year> <day> <subcommand> <subcommand args>...: dispatches to a specific solution.
    note that even if that solution doesn't exist, `new` command will still be available.
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
