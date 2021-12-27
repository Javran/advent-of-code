{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2019.Day17Spec
  ( spec
  )
where

import Control.Monad
import Data.Maybe
import Javran.AdventOfCode.Y2019.Day17
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

{-
  TOOD: it might be useful that we do this `describe "year" $ describe "day"` thing
  by utilizing Solution instance.
 -}

{-
  We assume that the program does not need us to do any merging / splitting. e.g:

  - turn `Forward 1 then Forward 2` into `Forward 3` or the other way around
  - `TurnRight` 3 times is the same as `TurnLeft`

  So we want to generate arbitrary programs that is a sequence of
  alternating between forward and turns.

 -}
forwardThenTurn, turnThenforward :: Gen [Move]
(forwardThenTurn, turnThenforward) =
  ( sequence [fwd, turn]
  , sequence [turn, fwd]
  )
  where
    turn = oneof $ fmap pure [TurnLeft, TurnRight]
    fwd =
      Forward
        <$>
        {-
          Just do 1-9 so we know the length is always 1.
          -}
        chooseInt (1, 9)

{-
  Generates a main routinue that is guaranteed to have a possible valid breakdown.
 -}
genMainMoves
  :: Gen
       ( [Move] -- the main routine
       , -- how we break it down
         ([Move], [Move], [Move])
       )
genMainMoves = do
  -- be consistent about how we alternate.
  (genPair :: Gen [Move]) <- oneof [pure forwardThenTurn, pure turnThenforward]
  {-
    raw instruction length <= 20, meaning for
    a sequence of Moves xs, we want `length xs` <= 10,
    with comma seperators, we get a length of 19,
    (remember every move we generate are guaranteed to have length 1).
    which is the longest we can have.
    Since we are talking about pairs, we want at most 5 of them here.
   -}
  subs <- replicateM 3 do
    n <- chooseInt (1, 5)
    rs <- replicateM n genPair
    pure $ concat rs
  let subA, subB, subC :: [Move]
      [subA, subB, subC] = subs
  n <- chooseInt (1, 10)
  xs <- concat <$> replicateM n (oneof (fmap pure [subA, subB, subC]))
  pure (xs, (subA, subB, subC))

spec :: Spec
spec =
  describe "Y2019" $
    describe "Day17" $
      prop "breakIntoRoutines" do
        {-
          This property only seek to verify that all results
          that our algorithm generates are valid break down
          of the original one.
          However, there are still cases that our algorithm can't
          find a solution,  but I'm happy with the status quo
          that we choose to not to worry about them here.
         -}
        forAll genMainMoves \(moves, _subs) -> do
          let results = breakIntoRoutines "XYZ" moves
              solvingStatus =
                if null results
                  then "unsolved"
                  else "solved"
          label solvingStatus $
            conjoin @Property
              (fmap
                 (\(mainRoutine, progList) -> do
                    let expanded :: [Move]
                        expanded =
                          concatMap
                            (\subName ->
                               fromJust $ lookup subName progList)
                            mainRoutine
                    expanded === moves)
                 results)
