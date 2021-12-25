{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Javran.AdventOfCode.Y2020.Day16
  (
  )
where

import Control.Monad
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP

data Day16 deriving (Generic)

type Range = (Int, Int)

type FieldRangeInfo = (String, [Range])

rangeP :: ReadP Range
rangeP = (,) <$> decimal1P <*> (char '-' *> decimal1P)

fieldRangeInfoP :: ReadP FieldRangeInfo
fieldRangeInfoP =
  (,)
    <$> (munch1 (/= ':') <* string ": ")
    <*> (rangeP `sepBy` string " or ")

solve :: IM.IntMap IS.IntSet -> [(Int, Int)] -> [] [(Int, Int)]
solve clues solution
  | IM.null clues = [solution]
  | otherwise = do
    let (k, vs) = minimumBy (comparing (IS.size . snd)) $ IM.toList clues
    v <- IS.toList vs
    let clues' = fmap (IS.delete v) $ IM.delete k clues
    guard $ not (any IS.null clues')
    solve clues' ((k, v) : solution)

instance Solution Day16 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    let parseInts :: String -> [Int]
        parseInts = fmap read . splitOn ","
    [ xs
      , ["your ticket:", ys]
      , "nearby tickets:" : zs
      ] <-
      splitOn [""] . lines <$> getInputS
    let rangeRules = fmap (fromJust . consumeAllWithReadP fieldRangeInfoP) xs
        yourTicket = parseInts ys
        nearbyTickets = fmap parseInts zs
        allRanges = concatMap snd rangeRules
        isInvalidVal v = not (any (\r -> inRange r v) allRanges)
        invalidVals = filter isInvalidVal $ concat nearbyTickets
    answerShow (sum invalidVals)
    let invalids = IS.fromList invalidVals
        validTickets =
          yourTicket :
          filter (all (`IS.notMember` invalids)) nearbyTickets
        fieldVals = fmap IS.fromList $ transpose validTickets
        -- collect possible relations:
        -- key is field index and value is list of possible val sets.
        possiblePairs = IM.fromListWith IS.union $ do
          (indField, (_fName, ranges)) <- zip [0 :: Int ..] rangeRules
          (indVal, vs) <- zip [0 :: Int ..] fieldVals
          guard $ all (\v -> any (\r -> inRange r v) ranges) (IS.toList vs)
          pure (indField, IS.singleton indVal)
        solution =
          -- just need one solution, probably the only one.
          head $ solve possiblePairs []

    answerShow $
      product $ do
        (indField, (fName, _)) <- zip [0 :: Int ..] rangeRules
        guard $ "departure" `isPrefixOf` fName
        let Just colInd = lookup indField solution
        pure $ yourTicket !! colInd
