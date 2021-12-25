module Javran.AdventOfCode.Y2019.Day25.Common
  ( Dir (..)
  , oppositeDir
  , PressureAnalysisResult (..)
  , RoomInfo (..)
  , SimpleResponse (..)
  , Response (..)
  )
where

import Javran.AdventOfCode.Prelude

data Dir
  = North
  | West
  | South
  | East
  deriving (Bounded, Enum, Show)

oppositeDir :: Dir -> Dir
oppositeDir d = allDirs !! (fromEnum d + 2)
  where
    allDirs = cycle universe

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
