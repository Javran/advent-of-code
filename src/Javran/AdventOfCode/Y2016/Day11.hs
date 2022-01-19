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
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
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
  forM_ (zip [4, 3, 2, 1] (reverse floors)) \(i, fl) -> do
    let objs =
          ["E" | ev == i] <> do
            (k, vs) <- IM.toAscList fl
            v <- vs
            pure $
              (take 3 $ iToS k) <> "-" <> case v of
                Generator -> "G"
                Microchip -> "M"
    putStrLn $ "F" <> show i <> ": " <> intercalate ", " objs

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
          ( 1
          , fmap (IM.fromListWith (<>) . fmap (\(k, v) -> (sToI k, [v]))) inp
          )
    pprWorld iToS initSt
