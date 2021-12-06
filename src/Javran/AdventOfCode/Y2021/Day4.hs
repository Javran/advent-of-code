{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2021.Day4
  (
  )
where

import Control.Monad.State.Strict
import Data.Bifunctor
import qualified Data.IntSet as IS
import Data.List
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude

data Day4 deriving (Generic)

type Board = [[Int]]

type BingoSet = [IS.IntSet]

bingoSet :: Board -> BingoSet
bingoSet bd = fmap IS.fromList bd <> fmap IS.fromList (transpose bd)

type TracedBingoSet = (Int, BingoSet)

type GameState =
  ( IS.IntSet {- called numbers -}
  , [TracedBingoSet {- bingo set of boards that have not won -}]
  )

type WinnerInfo =
  ( Int {- which board -}
  , ( Int {- sum of unmarked numbers -}
    , Int {- number just called -}
    )
  )

callNumber :: (Int -> Board) -> Int -> State GameState [WinnerInfo]
callNumber getBoard n = do
  modify (first (IS.insert n))
  (calledNums, tracedBingoSets) <- get
  let winners = do
        (w, bSets) <- tracedBingoSets
        guard $ any (\b -> IS.null (b `IS.difference` calledNums)) bSets
        pure w
      winnerSet = IS.fromList winners
  modify (second (filter ((`IS.notMember` winnerSet) . fst)))
  pure $ do
    winner <- winners
    let winnerBoard = getBoard winner
        unmarkedNums = filter (`IS.notMember` calledNums) $ concat winnerBoard
    pure (winner, (sum unmarkedNums, n))

instance Solution Day4 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    [callSeqRaw] : bdsRaw <- splitOn [""] . lines <$> getInputS
    let callSeq :: [Int]
        callSeq = fmap read . splitOn "," $ callSeqRaw
        bds :: [Board]
        bds = (fmap . fmap) (fmap read . words) bdsRaw
        tracedBingoSets = zipWith (\i bd -> (i, bingoSet bd)) [0 ..] bds
        history =
          concat $
            evalState
              (mapM (callNumber getBoard) callSeq)
              (IS.empty, tracedBingoSets)
          where
            getBoard which = bds !! which
        computeScore (_w, (u, v)) = u * v
    answerShow (computeScore (head history))
    answerShow (computeScore (last history))
