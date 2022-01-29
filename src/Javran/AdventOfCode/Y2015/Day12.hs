{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2015.Day12
  (
  )
where

import Data.Aeson
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Function
import qualified Data.HashMap.Strict as HM
import Data.Scientific
import Javran.AdventOfCode.Prelude

data Day12 deriving (Generic)

collectNums :: (Object -> Bool) -> Value -> Scientific
collectNums shouldCollect = fix \go -> \case
  Object o ->
    if shouldCollect o
      then sum $ fmap go o
      else 0
  Array vs -> sum $ fmap go vs
  Number n -> n
  _ -> 0

instance Solution Day12 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    raw <- BSC.pack <$> getInputS
    let Just o = decode' @Value (BSL.fromStrict raw)
    do
      let Right ans1 =
            floatingOrInteger @Double @Int (collectNums (\_ -> True) o)
      answerShow ans1
    do
      let shouldCollect v =
            not (HM.member "red" v) && ("red" `notElem` HM.elems v)
          Right ans2 =
            floatingOrInteger @Double @Int (collectNums shouldCollect o)
      answerShow ans2
