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

module Javran.AdventOfCode.Y2019.Day21
  (
  )
where

{-
  TODO: below are some initial ideas without further looking into detail.

  Notice that the machine can only have a very limited space of state:
  we have 6 registers, all of which are booleans, meanihg 2^6 = 64 states.

  So I suspect this is sort of like day 15 (in which we detect details
  about a 2d map) but here we are detecting a truth table instead.

  One limitation is that we have at most 15 instructions to use,
  so probably some optimization needed to encode this truth table.

  This might be of interest (putting the link here just so that I don't
  forget what it's called again):

  https://en.wikipedia.org/wiki/Karnaugh_map

  Update: forget about Karnaugh map, it does not scale to variables >= 6.

  However we have some constraints that might be interesting:

  Notice that while the truth table of AND and OR depends on
  the values of both of the input registers, NOT completely ignores
  what value is in Y, meaning it could destroy information so
  if we want to get some negated read from a register,
  NOT must be the first instruction to read from it.

  In addition, although this is not explicitly specified that *when* does
  register J and T reset to false, but I suspect it means every turn,
  meaning we can't take advange of "hidden states" - say we have
  two different situations where all read-only registers are the same
  but T is different - this assumption actually limits the solution space
  quite a lot.

 -}

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Bool
import Data.Char
import Data.Either
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2019.IntCode
import Text.ParserCombinators.ReadP hiding (count, many)

data Day21 deriving (Generic)

{-
  TODO: I don't have a good idea of how to solve this in a general way for now,
  but I think the best we can do is to have some tools
  to help the process of getting one solution and gradually
  automate what we can.
 -}

{-

if we do nothing, we fall at this scenario:

> #####@###########

So we issue `NOT A J`, which should allow us to jump here:

> @
> # ###
>  ABCD

Next failure:

> #####..#@########

Meaning we should not jump here:

> @
> #..#.
>  ABCD

or, recognizing jumping a bit earlier will allow us to
landing on a safe block, we should jump here:

> @
> ##..#
>  ABCD

So `OR D J`, but we now have two branches.
making room by using T:

- NOT A J
- OR D T
- OR T J

Next failure:

> #####..#@########

We jumped too eagerly here:

> @
> #####
>  ABCD

Probably putting in a condition that C should be missing:

- NOT A J
- NOT C T
- AND D T
- OR T J

notice how NOT must be the first one of each "AND-section"

 -}
p1Input :: [String]
p1Input =
  [ "NOT A J"
  , "NOT C T"
  , "AND D T"
  , "OR T J"
  , "WALK"
  ]

{-
  Starting from scratch, but `NOT A J` looks good to keep.

Failure:

> #####..#@########

let's make it jump when D is present

> @
> ##..#.####
>  ABCDEFGHI

- NOT A J
- OR D T
- OR T J

... I get the feeling that this is exactly p1,
let's just use the stuff there...

also let's change notation.

say in p1 we have:

> !A + !C*D

should not jump:

> @..............
> ###.#.#..######
>  ABCDEFGHI

> !A + !C*D*H

should not jump:

> @............
> #.##.#.######
>  ABCDEFGHI

> @
> ##.##.#.##
>  ABCDEFGHI

> !A + !C*D*H + !B*F

should not jump:

> @.........
> ##...#####
>  ABCDEFGHI

> !A + !C*D*H + !B*D*F

should jump:

> @............
> #.##...##.###
>  ABCDEFGHI

> @...?...?
> ##.##...##.###
>  ABCDEFGHI

> !A + !C*D*H + !B*D*F + !B*D*H

 -}
p2Input :: [String]
p2Input =
  [ "NOT A J"
  , "NOT C T"
  , "AND D T"
  , "AND H T"
  , "OR T J"
  , "NOT B T"
  , "AND D T"
  , "AND F T"
  , "OR T J"
  , "NOT B T"
  , "AND D T"
  , "AND H T"
  , "OR T J"
  , "RUN"
  ]

{-
  TODO: find something not that manual...
 -}

instance Solution Day21 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    code <- parseCodeOrDie <$> getInputS
    let runPart1 = True
        runPart2 = True
    when runPart1 do
      (_, out) <- runProgram code (fmap ord $ unlines p1Input)
      let isSolved = last out > 0x7f
      if isSolved
        then answerShow (last out)
        else putStrLn (fmap chr out)
    when runPart2 do
      (_, out) <- runProgram code (fmap ord $ unlines p2Input)
      let isSolved = last out > 0x7f
      if isSolved
        then answerShow (last out)
        else putStrLn (fmap chr out)
