{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

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
 -}

import qualified Data.IntMap.Strict as IM
import Javran.AdventOfCode.Cli.New
import Javran.AdventOfCode.Infra
import Javran.AdventOfCode.MainMaker
import Javran.AdventOfCode.Solutions
import System.Environment
import System.Exit

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

subCommand :: SubCmdContext -> CommandMode -> IO ()
subCommand ctxt mode = do
  let SubCmdContext {cmdHelpPrefix = _unused} = ctxt
  case mode of
    ModeList _ ->
      mapM_ (print . (\(SomeSolution s) -> solutionIndex s)) allSolutionsSorted
    ModeYear year _ ->
      case allSolutions IM.!? year of
        Nothing -> do
          putStrLn "No solution available, valid years:"
          print (IM.keys allSolutions)
        Just yearSols ->
          mapM_ (print . (\(_, SomeSolution s) -> solutionIndex s)) (IM.toAscList yearSols)
    ModeYearDay year day xs ->
      case getSolution year day of
        Just ss ->
          withArgs xs $ runSomeSolution ss ctxt
        Nothing ->
          case xs of
            ["new"] -> newCommandForYearDay year day
            _ ->
              die "No solution available, only `new` command is accepted."
