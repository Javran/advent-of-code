{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.ColorfulTerminal
  ( OutputMethod
  , getOutputMethod
  , TermOutputMode(..)
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

type OutputMethod = Either (String -> IO ()) TermOutputMode

data TermOutputMode
  = BasicTerm (TermOutput -> IO ())
  | ColorTerm ColorfulTerminal

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
  Nothing -> Left testOutputer
  Just t ->
    Right $
      maybe
        (BasicTerm (runTermOutput t))
        ColorTerm
        (getColorfulTerminal t)
