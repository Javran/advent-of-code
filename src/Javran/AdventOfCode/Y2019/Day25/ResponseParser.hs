{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2019.Day25.ResponseParser
  ( responsesP
  )
where

import Control.Monad
import Control.Monad.Combinators
import Data.Char
import Data.List
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2019.Day25.Common
import Text.ParserCombinators.ReadP hiding (count, get, many, manyTill)

dirP :: ReadP Dir
dirP =
  foldl1'
    (<++)
    [ North <$ string "north"
    , West <$ string "west"
    , South <$ string "south"
    , East <$ string "east"
    ]

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
        msg <- munch1 (/= '\n') <* char '\n'
        if
            | "heavier" `isInfixOf` msg ->
              pure $ Just $ ParFailure False
            | "lighter" `isInfixOf` msg ->
              pure $ Just $ ParFailure True
            | "You may proceed" `isInfixOf` msg -> do
              -- "Santa notices your small droid, ..."
              _ <- munch1 (/= '\n') <* char '\n'
              _ <- munch1 (not . isDigit)
              ans <- decimal1P
              _ <- munch1 (/= '\n') <* char '\n'
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

responsesP :: ReadP [Response]
responsesP =
  manyTill
    ((string "\n\n\n" *> (RespRoomInfo <$> roomInfoP))
       <|> (RespSimple <$> simpleResponseP))
    (string "Command?\n"
       <++ (do
              [] <- look
              pure []))
