{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Javran.AdventOfCode.Y2019.Day25.Common
  ( Dir (..)
  , PressureAnalysisResult (..)
  , RoomInfo (..)
  , SimpleResponse (..)
  , Response (..)
  , AsciiResult (..)
  , asciiRun
  , dirToCmd
  , module Javran.AdventOfCode.GridSystem.RowThenCol.Nwse
  )
where

import Data.Char
import Javran.AdventOfCode.Y2019.IntCode
import Javran.AdventOfCode.GridSystem.RowThenCol.Nwse

data PressureAnalysisResult
  = ParFailure {parFailureShouldBeLighter :: Bool}
  | ParSuccess Int
  deriving (Show)

data RoomInfo = RoomInfo
  { riName :: String
  , riDescription :: String
  , riDoorsHereLead :: [Dir]
  , riItemsHere :: [String]
  , riPressureSensitiveExtra :: Maybe PressureAnalysisResult
  }
  deriving (Show)

data SimpleResponse
  = SimpRespCan'tGoThatWay
  | SimpRespDon'tSeeThatItem
  | SimpRespDon'tHaveThatItem
  | SimpRespInventory [String]
  | SimpRespTakeOrDropItem
      { simplRespIsTaking :: Bool
      , simplRespItem :: String
      }
  deriving (Show)

data Response
  = RespRoomInfo RoomInfo
  | RespSimple SimpleResponse
  deriving (Show)

dirToCmd :: Dir -> String
dirToCmd = \case
  North -> "north"
  West -> "west"
  South -> "south"
  East -> "east"

{-
  TODO: backport those to IntCode module.

  Note that we defintely need to change interface a bit
  as in Day 25 it seems to be safe to assume that everything
  is ASCII-based, but in other problems one might output
  a number outside of ASCII range, that we have to handle differently.
 -}
takeAllOutputs :: IO (Result a) -> IO ([Int], IO (Result a))
takeAllOutputs prog = takeAllAux [] prog
  where
    takeAllAux accRev p = do
      result <- p
      case result of
        SentOutput o k ->
          takeAllAux (o : accRev) k
        _ -> pure (reverse accRev, pure result)

data AsciiResult
  = AsciiDone
  | AsciiNeedCommand (String -> IO AsciiResult)
  | AsciiOutput String (IO AsciiResult)

asciiRun :: IO (Result a) -> IO AsciiResult
asciiRun prog = do
  result <- prog
  case result of
    Done _ -> pure AsciiDone
    NeedInput {} -> pure $ AsciiNeedCommand \cmd -> do
      -- note that asciiRun enters '\n' for us.
      ([], k) <- communicate (fmap ord cmd <> [10]) 0 (pure result)
      asciiRun k
    SentOutput {} -> do
      {-
        unlike inputs, here we choose to take all outputs without spliting
        it to lines - we could do that, but it's probably better to preseve
        info and let client decide what to do, which could include passing it
        to a more sophisticated parser than the simple line splitting that
        we have here.
       -}
      (outs, k) <- takeAllOutputs (pure result)
      pure $ AsciiOutput (fmap chr outs) (asciiRun k)
