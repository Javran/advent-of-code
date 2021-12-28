{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Javran.AdventOfCode.Y2018.Day4
  (
  )
where

import Control.Monad
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Monoidal.Strict as MMap
import Data.Monoid
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day4 deriving (Generic)

type Date = (Int, Int, Int)

type DateTime = (Date, (Int, Int))

type Event = Either Int Bool {- False: asleep, True: awake -}

type EventLog = (DateTime, Event)

eventLogP :: ReadP EventLog
eventLogP = do
  date <-
    (,,)
      <$> (char '[' *> decimal1P)
      <*> (char '-' *> decimal1P)
      <*> (char '-' *> decimal1P)
  time@(hh, _mm) <-
    (,)
      <$> (char ' ' *> decimal1P)
      <*> (char ':' *> decimal1P)
  _ <- string "] "
  e <-
    (Right False <$ string "falls asleep")
      <++ (Right True <$ string "wakes up")
      <++ (Left <$> (string "Guard #" *> decimal1P <* string " begins shift"))
  guard case e of
    Right _ ->
      -- confirm that "falls asleep" and "wakes up" always happen at 00:XX.
      hh == 0
    Left _ -> True
  pure ((date, time), e)

type ShiftInfo = (Int, [(Int, Int)]) -- guard id, list of (asleep, awake)

{-
  Splits sorted EventLog into each individual shifts and removes irelevant info
 -}
splitLog :: [EventLog] -> [ShiftInfo]
splitLog = chop f
  where
    f = \((_, Left n) : xs) ->
      let (mSleepWake, v) = span (isRight . snd) xs
          isAlternating = and (zipWith (==) (cycle [False, True]) bools)
          (bools, times) =
            unzip $
              fmap
                (\((_, (_, mm)), sleepWake) -> case sleepWake of
                   Right b -> (b, mm)
                   Left _ -> unreachable)
                mSleepWake
          asleepAwakes = fmap (\[x, y] -> (x, y)) $ chunksOf 2 times
       in if isAlternating then ((n, asleepAwakes), v) else errInvalid

gatherInfo :: ShiftInfo -> (Int, (Sum Int, MMap.MonoidalMap Int (Sum Int)))
gatherInfo = second (foldMap collect)
  where
    collect = \(x, y) ->
      ( -- accumulate total asleep minutes
        Sum $ y - x
      , -- freq count on all asleep minutes
        MMap.fromList $ fmap (,1 :: Sum Int) [x .. y -1]
      )

instance Solution Day4 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- sortBy (comparing fst) . fmap (consumeOrDie eventLogP) . lines <$> getInputS
    let ys = fmap gatherInfo $ splitLog xs
        freqCounted = MMap.filter ((/= 0) . fst) $ MMap.fromListWith (<>) ys
    do
      let (who, (_, freqCount)) =
            maximumBy (comparing (fst . snd)) $ MMap.toList freqCounted
          (mm, _) = maximumBy (comparing snd) $ MMap.toList freqCount
      answerShow $ who * mm
    do
      let (who, (mm, _)) =
            maximumBy (comparing (snd . snd))
              . MMap.toList
              . MMap.map (\(_, m) -> maximumBy (comparing snd) $ MMap.toList m)
              $ freqCounted
      answerShow $ who * mm
