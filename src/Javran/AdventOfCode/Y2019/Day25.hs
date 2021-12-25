{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2019.Day25
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Control.Monad.Combinators
import Data.Char
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Debug.Trace (traceShow)
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2019.IntCode
import Text.ParserCombinators.ReadP hiding (count, get, many, manyTill)

data Day25 deriving (Generic)

allItems :: [String]
allItems =
  [ "cake"
  , "fuel cell"
  , "easter egg"
  , "ornament"
  , "hologram"
  , "dark matter"
  , "klein bottle"
  , "hypercube"
  ]

presetCommands :: [String]
presetCommands =
  [ "north"
  , "north"
  , "east"
  , "east" -- at Crew Quarters
  , "take cake"
  , "west"
  , "west"
  , "south"
  , "south" -- at Hull Breach
  , "south"
  , "west" -- at Hallway
  , "take fuel cell"
  , "west" -- at Warp Drive Maintenance
  , "take easter egg"
  , "east"
  , "east"
  , "north" -- at Hull Breach
  , "east"
  , "take ornament"
  , "east"
  , "take hologram"
  , "east"
  , "take dark matter"
  , "north"
  , "north"
  , "east"
  , "take klein bottle"
  , "north"
  , "take hypercube"
  , "north" -- at Security Checkpoint
  ]
    <> dropEverything
    <> tryAllCombinations
  where
    dropEverything = ["drop " <> x | x <- allItems]

    allCombinations = filterM (const [False, True]) allItems

    tryComb :: [String] -> [String]
    tryComb items =
      ["take " <> item | item <- items] <> ["west"]
        <> ["drop " <> item | item <- items]
    tryAllCombinations = do
      comb <- allCombinations
      tryComb comb

takeAllOutputs :: IO (Result a) -> IO ([Int], IO (Result a))
takeAllOutputs prog = takeAllAux [] prog
  where
    takeAllAux accRev p = do
      result <- p
      case result of
        SentOutput o k ->
          takeAllAux (o : accRev) k
        _ -> pure (reverse accRev, pure result)

data Dir = North | West | South | East deriving (Bounded, Enum, Show)

dirP :: ReadP Dir
dirP =
  foldl1'
    (<++)
    [ North <$ string "north"
    , West <$ string "west"
    , South <$ string "south"
    , East <$ string "east"
    ]

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

roomInfoP :: ReadP RoomInfo
roomInfoP = do
  riName <- string "== " *> manyTill (satisfy (/= '\n')) (string " ==\n")
  riDescription <- munch1 (/= '\n') <* string "\n\n"
  _ <- string "Doors here lead:\n"
  riDoorsHereLead <- manyTill (string "- " *> dirP <* char '\n') (char '\n')
  riItemsHere <- do
    xs <- look
    if "Items here:\n" `isPrefixOf` xs
      then do
        _ <- string "Items here:\n"
        manyTill (string "- " *> munch1 (/= '\n') <* char '\n') (char '\n')
      else pure []
  riPressureSensitiveExtra <-
    if riName == "Pressure-Sensitive Floor"
      then do
        xs <- look
        guard $ "A loud," `isPrefixOf` xs
        msg <- (munch1 (/= '\n') <* char '\n')
        if
            | "heavier" `isInfixOf` msg ->
              pure $ Just $ ParFailure False
            | "lighter" `isInfixOf` msg ->
              pure $ Just $ ParFailure True
            | "You may proceed" `isInfixOf` msg -> do
              -- "Santa notices your small droid, ..."
              _ <- (munch1 (/= '\n') <* char '\n')
              _ <- munch1 (not . isDigit)
              ans <- decimal1P
              _ <- (munch1 (/= '\n') <* char '\n')
              pure $ Just $ ParSuccess ans
            | otherwise -> pfail
      else pure Nothing
  pure
    RoomInfo
      { riName
      , riDescription
      , riDoorsHereLead
      , riItemsHere
      , riPressureSensitiveExtra
      }

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

simpleResponseP :: ReadP SimpleResponse
simpleResponseP =
  (do
     _ <- string "\nYou "
     let takeOrDropP = do
           simplRespIsTaking <- (True <$ string "take the ") <++ (False <$ string "drop the ")
           simplRespItem <- munch (/= '.')
           pure SimpRespTakeOrDropItem {simplRespIsTaking, simplRespItem}
     r <-
       (SimpRespCan'tGoThatWay <$ string "can't go that way")
         <++ (SimpRespDon'tSeeThatItem <$ string "don't see that item here")
         <++ (SimpRespDon'tHaveThatItem <$ string "don't have that item")
         <++ (SimpRespInventory [] <$ string "aren't carrying any items")
         <++ takeOrDropP
     _ <- string ".\n\n"
     pure r)
    <++ do
      _ <- string "\nItems in your inventory:\n"
      xs <- manyTill (string "- " *> munch1 (/= '\n') <* char '\n') (char '\n')
      pure $ SimpRespInventory xs

data Response
  = RespRoomInfo RoomInfo
  | RespSimple SimpleResponse
  deriving (Show)

responseP :: ReadP [Response]
responseP =
  manyTill
    ((string "\n\n\n" *> (RespRoomInfo <$> roomInfoP))
       <|> (RespSimple <$> simpleResponseP))
    (string "Command?\n"
       <++ (do
              [] <- look
              pure []))

debugConsumeAllWithReadP :: ReadP a -> String -> Either String a
debugConsumeAllWithReadP p xs = case readP_to_S (p <* eof) xs of
  [] -> Left "No alternatives."
  [(v, "")] -> pure v
  [(_, unconsumed@(_ : _))] -> Left $ "Consumed successfully but with leftover: " <> unconsumed
  (_ : _) -> Left $ "Parsing is ambiguious."

{-
  TODO:

  the sequence of preset command only works on my specific input,
  we need few parts to be more flexible to solve this puzzle in general:

  - room and item detection
  - now we can head to Security Checkpoint
  - bruteforce or do something smarter.

 -}
instance Solution Day25 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    code <- parseCodeOrDie <$> getInputS
    let prog = void <$> startProgramFromFoldable code
        shouldRunProgram = True
    when shouldRunProgram do
      fix
        (\loop curP curPresetCmds -> do
           result <- curP
           case result of
             Done _ -> pure ()
             NeedInput {} -> do
               (cmd, nextPresetCmds) <- case curPresetCmds of
                 [] -> do
                   xs <- getLine
                   pure (xs, [])
                 (pCmd : cs') -> do
                   putStrLn $ "Enter preset command: " <> pCmd
                   pure (pCmd, cs')
               ([], k) <- communicate (fmap ord cmd <> [10]) 0 (pure result)
               loop k nextPresetCmds
             SentOutput {} -> do
               (outs, k') <- takeAllOutputs (pure result)
               let r = debugConsumeAllWithReadP responseP (fmap chr outs)
               case r of
                 Right v -> print v
                 Left msg -> do
                   putStrLn $ "warning: parse failure, reason: " <> msg
                   putStr (fmap chr outs)
               loop k' curPresetCmds)
        prog
        presetCommands
