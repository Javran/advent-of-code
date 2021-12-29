{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}

module Javran.AdventOfCode.Y2018.Day9
  (
  )
where

import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IM
import Data.STRef
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day9 deriving (Generic)

puzzleP :: ReadP (Int, Int)
puzzleP =
  (,) <$> (decimal1P <* string " players; last marble is worth ")
    <*> (decimal1P <* string " points")

data Node s = Node
  { nLabel :: Int
  , nPrev :: STRef s (Node s)
  , nNext :: STRef s (Node s)
  }

type PlayerScores = IM.IntMap Int

playGame :: Int -> Int -> PlayerScores
playGame playerCount lastMarble =
  runST $ flip evalStateT (IM.fromList (fmap (,0) [1 .. playerCount])) mdo
    let getNext r = nNext <$> lift (readSTRef r)
        getPrev r = nPrev <$> lift (readSTRef r)
        getPrev7 = foldr1 (>=>) (replicate 7 getPrev)
        newMarble marble cur = do
          next0 <- getNext cur
          next1 <- getNext next0
          lift do
            new <- newSTRef Node {nLabel = marble, nPrev = next0, nNext = next1}
            modifySTRef' next0 (\n -> n {nNext = new})
            modifySTRef' next1 (\n -> n {nPrev = new})
            pure new
        scoreMarble marble who cur = do
          p7 <- getPrev7 cur
          (v, newCur) <- lift do
            Node {nPrev = p7Prev, nNext = p7Next, nLabel = v} <- readSTRef p7
            modifySTRef' p7Prev (\n -> n {nNext = p7Next})
            modifySTRef' p7Next (\n -> n {nPrev = p7Prev})
            pure (v, p7Next)
          modify (IM.adjust (+ (marble + v)) who)
          pure newCur
        players = cycle [1 .. playerCount]
        marbles = [1 .. lastMarble]
        play who marble cur =
          if marble `rem` 23 == 0
            then scoreMarble marble who cur
            else newMarble marble cur

    r0 <- lift $ newSTRef Node {nLabel = 0, nPrev = r0, nNext = r0}
    rCur <-
      foldM (\curR (who, marble) -> play who marble curR) r0 (zip players marbles)

    let _collect rStart = do
          fix
            (\loop r acc -> do
               Node {nLabel = v} <- lift (readSTRef r)
               r' <- getNext r
               if r' == rStart
                 then pure (reverse (v : acc))
                 else loop r' (v : acc))
            rCur
            []
    get

instance Solution Day9 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (playerCount, lastMarble) <- consumeOrDie puzzleP . head . lines <$> getInputS
    answerShow $ maximum $ playGame playerCount lastMarble
    answerShow $ maximum $ playGame playerCount (lastMarble * 100)
