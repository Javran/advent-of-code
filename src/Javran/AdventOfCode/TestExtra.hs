{-# LANGUAGE LambdaCase #-}

module Javran.AdventOfCode.TestExtra
  ( consumeExtra
  , shouldRun
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
