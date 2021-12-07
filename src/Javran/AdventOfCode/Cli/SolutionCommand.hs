{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Cli.SolutionCommand
  ( parse
  , CommandMode (..)
  , subCommand
  )
where

{-
  TODO: this will handle all solution-specific commands in the future,
  rather than having each year as a seperated command.

  Plan:

  - import update automation for Javran.AdventOfCode.Solutions
  - parse command line args in this module, dispatch to specific solutions or actions.

  Parsing:

  - "ls" for listing.
  - any integer value (<year>) commits to this subcommand.
  - <year>: list avaliable day solutions for that year
  - <year> <day> <subcommand> <subcommand args>...: this should preserve
    the old behavior.

 -}

import Control.Monad
import qualified Data.IntMap.Strict as IM
import Javran.AdventOfCode.Infra
import Javran.AdventOfCode.MainMaker
import Javran.AdventOfCode.Solutions
import System.Environment

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
  let SubCmdContext {cmdHelpPrefix} = ctxt
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
          -- no solution for this, but we can probably accept a "new" command.
          putStrLn "TODO"
