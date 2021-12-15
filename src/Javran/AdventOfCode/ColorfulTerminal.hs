{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.ColorfulTerminal
  ( OutputMethod (..)
  , getOutputMethod
  , ColorfulTerminal (..)
  , getColorfulTerminal
  , termText
  , threadDelay
  , module System.Console.Terminfo.Color
  , TermOutput
  )
where

import Control.Concurrent
import System.Console.Terminfo
import System.Console.Terminfo.Color

data OutputMethod
  = OutputForTest (String -> IO ())
  | OutputBasicTerm (TermOutput -> IO ())
  | OutputColorTerm ColorfulTerminal

data ColorfulTerminal = ColorfulTerminal
  { setBackground :: Color -> TermOutput -> TermOutput
  , setForeground :: Color -> TermOutput -> TermOutput
  , runTermOut :: TermOutput -> IO ()
  }

getColorfulTerminal :: Terminal -> Maybe ColorfulTerminal
getColorfulTerminal t = do
  (setForeground, setBackground) <-
    getCapability
      t
      ((,) <$> withForegroundColor @TermOutput
         <*> withBackgroundColor @TermOutput)
  pure
    ColorfulTerminal
      { setForeground
      , setBackground
      , runTermOut = runTermOutput t
      }

getOutputMethod
  :: (String -> IO ()) -> Maybe Terminal -> OutputMethod
getOutputMethod testOutputer = \case
  Nothing -> OutputForTest testOutputer
  Just t ->
    maybe
      (OutputBasicTerm (runTermOutput t))
      OutputColorTerm
      (getColorfulTerminal t)
