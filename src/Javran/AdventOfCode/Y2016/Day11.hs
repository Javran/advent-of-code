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

module Javran.AdventOfCode.Y2016.Day11
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Char
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Monoid
import qualified Data.PSQueue as PQ
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import GHC.IO (unsafePerformIO)
import Javran.AdventOfCode.Misc
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day11 deriving (Generic)

{-
  I'm not impressed by this poorly written problem description,
  it's almost like it is intentionally so to cause confusion.

  So to summarize and make things a bit more clear, here are my notes:

  - There are microchips (M) and generators (G), existing in pairs,
    in one-to-one correspondence (denoted as G-M pair).

  - When a G-M pair is on the same floor, they together
    forms a shield that protects the microchip.
    (whether either is inside of the elevator is irrelevant).

  - Generator G does damage to microchips M'
    when all of the following is true:

    - G is not paired with M'
    + M' is on the same floor
    + M' is not paired with it's generator G',
      as G'-M' sheilds M' from the damage.

  - Microchips does not interact with each other.

  - There's one elevator:

    + starting from floor 1 and move between 1-4.
    + only capable of moving up or down by 1 at a time.
    + must carry G or M for it to function
    + carries at most 2 of any combination of G or M.

  - The goal is to move everything to 4th floor without damaging
    to any microchips.

 -}

data Obj = Generator | Microchip deriving (Show, Eq, Ord)

inputP :: ReadP [[(String, Obj)]]
inputP = do
  let wordP = munch1 isAsciiLower
      objP =
        string "a " *> do
          w <- wordP
          o <-
            (Microchip <$ string "-compatible microchip")
              <++ (Generator <$ string " generator")
          pure (w, o)
      floorP =
        ([] <$ string "nothing relevant")
          <++ (do
                 a <- objP
                 ahead <- look
                 if
                     | "." `isPrefixOf` ahead ->
                       -- exactly one thing
                       pure [a]
                     | " and " `isPrefixOf` ahead -> do
                       -- two things
                       _ <- string " and "
                       b <- objP
                       pure [a, b]
                     | ", " `isPrefixOf` ahead -> do
                       -- three, four, or more things, oxford comma.
                       bs <-
                         between
                           (string ", ")
                           (string ", and ")
                           (objP `sepBy1` string (", "))
                       c <- objP
                       pure $ a : bs <> [c]
                     | otherwise -> pfail)
      dotNl = string ".\n"
  f1 <- between (string "The first floor contains ") dotNl floorP
  f2 <- between (string "The second floor contains ") dotNl floorP
  f3 <- between (string "The third floor contains ") dotNl floorP
  f4 <- between (string "The fourth floor contains ") dotNl floorP
  pure [f1, f2, f3, f4]

{-
  state of the world in a search space.

  (<level>, [<floor state>])

  - level: 0 ~ 3, where the elevator is, which is also where we are.
  - floors: from 1st to 4th.

 -}
type FloorState = IM.IntMap [Obj]

type WorldState = (Int, [FloorState])

pprWorld :: (Int -> String) -> WorldState -> IO ()
pprWorld iToS (ev, floors) = do
  forM_ (zip [3, 2, 1, 0] (reverse floors)) \(i, fl) -> do
    let objs =
          ["E" | ev == i] <> do
            (k, vs) <- IM.toAscList fl
            v <- vs
            pure $
              (take 3 $ iToS k) <> "-" <> case v of
                Generator -> "G"
                Microchip -> "M"
    putStrLn $ "F" <> show (i + 1) <> ": " <> intercalate ", " objs

isFloorSafe :: FloorState -> Bool
isFloorSafe fs = not hasGenerator || not hasUnprotected
  where
    hasGenerator = (any . any) (== Generator) fs
    hasUnprotected =
      any
        (\vs ->
           Microchip `elem` vs && Generator `notElem` vs)
        fs

step :: WorldState -> [] WorldState
step (ev, floors) = do
  ev' <- [ev -1, ev + 1]
  guard $ ev' >= 0 && ev' <= 3
  let flFrom = floors !! ev
      objs :: [(Int, Obj)]
      objs = do
        (k, vs) <- IM.toList flFrom
        v <- vs
        pure (k, v)
      flTo = floors !! ev'
  objsFrom <- do
    -- pick at least one object for the elevator to work.
    (obj0, os0) <- pickInOrder objs
    -- can optionally pick one extra object, or no extra at all.
    os <-
      (do
         obj1 <- os0
         pure [obj1])
        <|> pure []
    pure $ obj0 : os
  let deleteObj (i, o) = IM.adjust (delete o) i
      insertObj (i, o) = IM.insertWith (<>) i [o]
      flFrom' = foldr deleteObj flFrom objsFrom
      flTo' = foldr insertObj flTo objsFrom
  guard $ isFloorSafe flFrom' && isFloorSafe flTo'
  pure (ev', floors & ix ev .~ flFrom' & ix ev' .~ flTo')

{-
  For now let's just ignore where the elevator is
  and say if that floor contains anything, it needs to be moved
  to the 4-th floor and the underestimation is just that floor distance.

  Also that this esimation has the property that we are in a goal state
  iff. estimation says 0.
 -}
estimateDist :: WorldState -> Int
estimateDist (_, floors) =
  sum $ zipWith (\dist fs -> if null fs then 0 else dist) [3, 2, 1] floors

aStar ppr q0 gScores = case PQ.minView q0 of
  Nothing -> error "queue exhausted"
  Just (ws PQ.:-> (Arg (fScore :: Int) (dist :: Int)), q1) ->
    if fScore == dist
      then dist
      else
        let gScore = gScores M.! ws
            nexts = do
              ws' <- step ws
              let mGScore = gScores M.!? ws'
                  gScore' = gScore + 1
                  fScore' = gScore' + estimateDist ws'
                  dist' = dist + 1
              guard $ maybe True (gScore' <) mGScore
              pure (ws', gScore', Arg fScore' dist')
            q2 = foldr upd q1 nexts
              where
                upd (ws', _, prio') = PQ.insert ws' prio'
            gScores' = foldr upd gScores nexts
              where
                upd (ws', gScore', _) = M.insert ws' gScore'
         in unsafePerformIO do
              print ws
              putStrLn $ "cur: (dist=" <> show dist <> "):"
              ppr ws
              putStrLn "++++ nexts"
              forM_ nexts \(ws', _, _) -> do
                ppr ws'
                putStrLn ""
              putStrLn "--- nexts"
              putStrLn ""
              putStrLn ""
              pure $ aStar ppr q2 gScores'

instance Solution Day11 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    inp <- consumeOrDie inputP <$> getInputS
    let objTypes = do
          fl <- inp
          (t, _) <- fl
          pure t
        (sToI, iToS) = internalize objTypes
        initSt :: WorldState
        initSt =
          ( 0
          , fmap (IM.fromListWith (<>) . fmap (\(k, v) -> (sToI k, [v]))) inp
          )
    -- pprWorld iToS initSt
    let ans = aStar (pprWorld iToS) (PQ.singleton initSt (Arg (estimateDist initSt) 0)) (M.singleton initSt 0)
    print ans
    when False do
      let e =
            ( 4 -1
            , [ IM.empty
              , IM.empty
              , IM.fromList
                  [(0, [Microchip])]
              , IM.fromList
                  [(0, [Generator]), (1, [Microchip, Generator])]
              ]
            )
      pprWorld iToS e
      forM_ (step e) \w' -> do
        putStrLn "++++"
        print w'
        pprWorld iToS w'
        putStrLn "----\n"


{-

cur: (dist=9):
F4: E, hyd-G, lit-G, lit-M
F3: hyd-M
F2:
F1:
++++ nexts
F4: lit-G, lit-M
F3: E, hyd-G, hyd-M
F2: 
F1: 

--- nexts
 -}
