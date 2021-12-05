{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Javran.AdventOfCode.Y2020.Day23
  (
  )
where

import Control.Monad
import Control.Monad.ST
import Data.Char
import Data.Function
import Data.List
import Data.STRef
import qualified Data.Vector.Mutable as VM
import Javran.AdventOfCode.Prelude

data Day23

chInt :: Char -> Int
chInt v = ord v - ord '0'

intCh :: Int -> Char
intCh v = chr (v + ord '0')

{-
  head is always the current cup.
 -}
type CupState = [Int]

step :: CupState -> CupState
step = \case
  (foc : pre)
    | (xs, ys) <- splitAt 3 pre ->
      let downs = cycle [9, 8 .. 1]
          dest : _ = filter (`elem` ys) $ tail (dropWhile (/= foc) downs)
          (us, _dest : vs) = span (/= dest) ys
       in us <> (dest : xs) <> vs <> [foc]
  _ -> unreachable

toResult :: CupState -> String
toResult xs = fmap intCh $ take 8 $ tail $ dropWhile (/= 1) $ cycle xs

data Cup ref = Cup
  { _cupLabel :: Int
  , _cupNext :: ref (Cup ref)
  }

mixCups :: [Int] -> Int -> Int -> [Int]
mixCups xs len opCount = runST simulate
  where
    simulate :: forall s cup. cup ~ Cup (STRef s) => ST s [Int]
    simulate = do
      -- let vec[i] represent cup of label i+1.
      cups <- VM.unsafeNew len
      forM_ [1 .. len] $ \lbl -> do
        let i = lbl - 1
        r <- newSTRef (error $ "label " <> show lbl)
        VM.write cups i (Cup lbl r)
      let labels = xs <> [10 .. len]
          getCup lbl = VM.read cups (lbl -1)
      forM_ ((last labels, head labels) : zip labels (tail labels)) $ \(lblFrom, lblTo) -> do
        (Cup _ r) <- getCup lblFrom
        nTo <- getCup lblTo
        writeSTRef r nTo
      let performMove :: cup -> ST s cup
          performMove (Cup focLbl focRef) = do
            c1@(Cup lbl1 r1) <- readSTRef focRef
            (Cup lbl2 r2) <- readSTRef r1
            (Cup lbl3 r3) <- readSTRef r2
            cNext <- readSTRef r3
            -- removes c1,c2,c3
            writeSTRef focRef cNext
            let destCupLbl : _ =
                  filter (`notElem` [lbl1, lbl2, lbl3]) $
                    tail $ iterate nextDest focLbl
                  where
                    nextDest i = if i == 1 then len else i -1
            (Cup _ rDest) <- getCup destCupLbl
            -- insert
            afterDest <- readSTRef rDest
            writeSTRef rDest c1
            writeSTRef r3 afterDest
            -- return next cup to be operated upon.
            pure cNext
      getCup (head xs)
        >>= fix
          (\loop cnt cup ->
             unless (cnt == 0) do
               performMove cup >>= loop (cnt-1))
          opCount

      do
        (Cup _ r0) <- getCup 1
        (Cup lbl1 r1) <- readSTRef r0
        (Cup lbl2 _) <- readSTRef r1
        pure [lbl1, lbl2]

instance Solution Day23 where
  solutionIndex _ = (2020, 23)
  solutionRun _ SolutionContext {getInputS, answerShow, answerS} = do
    (extraOpts, rawInput) <- consumeExtraLeadingLines <$> getInputS
    let xs = fmap chInt . head . lines $ rawInput
    -- looks like a safe assumption, make sure of it.
    True <- pure (sort xs == [1 .. 9])
    let final = iterate step xs !! 100
    answerS (toResult final)
    let (len, opCount) = case extraOpts of
          Nothing -> (1000000, 10000000)
          Just [rawExtra] -> read rawExtra
          _ -> errInvalid
    answerShow $ product $ mixCups xs len opCount
