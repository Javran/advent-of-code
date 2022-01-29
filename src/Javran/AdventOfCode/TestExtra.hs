{-# LANGUAGE LambdaCase #-}

module Javran.AdventOfCode.TestExtra
  ( consumeExtra
  , shouldRun
  , singleLineExtra
  )
where

import Javran.AdventOfCode.Infra

{-
  Some tests may contain extra input data,
  this module provides some common utils for that.
 -}

consumeExtra :: IO String -> IO (Maybe [String], String)
consumeExtra getInpS = consumeExtraLeadingLines <$> getInpS

{-
  This function can be used to determine whether part 1 or part 2 should run,
  if that information is present in a line-based extra input.
 -}
shouldRun :: Maybe [String] -> (Bool, Bool)
shouldRun = \case
  Nothing -> (True, True)
  Just e -> ("part1" `elem` e, "part2" `elem` e)

{-
  Expects test's extra input to be exactly one line (if exists)
  and parses it using Read.
  If extra input doesn't exist, falls back to a defulat value.
 -}
singleLineExtra :: Read a => a -> Maybe [String] -> a
singleLineExtra d = \case
  Nothing -> d
  Just [x] -> read x
  Just xs -> error $ "Expected exactly one line in extra, got " <> show (length xs)
