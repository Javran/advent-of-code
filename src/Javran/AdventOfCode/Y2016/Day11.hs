{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2016.Day11
  (
  )
where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Bits
import Data.Char
import Data.Foldable
import qualified Data.IntMap.Strict as IM
import Data.List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.PSQueue as PQ
import Data.Semigroup
import Javran.AdventOfCode.Misc
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.TestExtra
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

type ParsedInput = [[(String, Obj)]]

inputP :: ReadP ParsedInput
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
                           (objP `sepBy1` string ", ")
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

  FloorState uses NonEmpty, as we want to make sure that when
  none of G or M are on that floor, we also remove that element's presence
  as map key as well.

 -}
type FloorState = IM.IntMap (NE.NonEmpty Obj)

type WorldState = (Int, [FloorState])

type WorldStateNorm = (Int, [Int])

{-
  There's an important observation under which lots of
  states can be considered duplicates and thus reduce the search space:

  Imagine, if we have:

  - X-G on floor a, X-M on floor b
  - Y-G on floor c, Y-M on floor d

  in which X and Y are distinct items,
  but a,b,c,d are allowed to be assigned same value.

  it is the same as:

  - Y-G on floor a, Y-M on floor b
  - X-G on floor c, X-M on floor d

  in other words, we don't care about the exact X or Y, the only thing matters
  is which floor contains the generator and which one contains the microchip.

  So, if we flatten and re-arrange the structure of [FloorState],
  make all objects keyed by their element type (i.e. X- or Y-),
  we can neatly encode the location (which are just floor numbers)
  of Gs and Ms by using just 8 bits, as an Int.

  After doing so, we can further cut down search space by sorting the resulting list -
  after all, we only care about the *bag* of all configurations.

 -}
normWorld :: WorldState -> WorldStateNorm
normWorld (ev, floors) = (ev, sort $ IM.elems compact)
  where
    compact :: IM.IntMap Int
    compact = IM.fromListWith (.|.) do
      (lvl, fs) <- zip [0 :: Int ..] floors
      (objTyp, objs) <- IM.toList fs
      {-
        Generator encodes to 0~3, Microchip encodes to 4~7.
       -}
      o <- toList objs
      let byObj = case o of
            Generator -> id
            Microchip -> \v -> unsafeShiftL v 4
      pure (objTyp, byObj (unsafeShiftL 1 lvl))

_pprWorld :: (Int -> String) -> WorldState -> IO ()
_pprWorld iToS (ev, floors) = do
  forM_ (zip [3, 2, 1, 0] (reverse floors)) \(i, fl) -> do
    let objs =
          ["E" | ev == i] <> do
            (k, vs) <- IM.toAscList fl
            v <- toList vs
            pure $
              take 3 (iToS k) <> "-" <> case v of
                Generator -> "G"
                Microchip -> "M"
    putStrLn $ "F" <> show (i + 1) <> ": " <> intercalate ", " objs

isFloorSafe :: FloorState -> Bool
isFloorSafe fs = not hasGenerator || not hasUnprotected
  where
    hasGenerator = any (elem Generator) fs
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
        v <- toList vs
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
  let deleteObj (i, o) =
        IM.alter
          (\case
             Nothing -> unreachable
             Just vs ->
               NE.nonEmpty $ delete o (toList vs))
          i
      insertObj (i, o) = IM.insertWith (<>) i (o NE.:| [])
      flFrom' = foldr deleteObj flFrom objsFrom
      flTo' = foldr insertObj flTo objsFrom
  guard $ isFloorSafe flFrom' && isFloorSafe flTo'
  pure (ev', floors & ix ev .~ flFrom' & ix ev' .~ flTo')

step' :: WorldState -> M.Map WorldStateNorm WorldState
step' ws = M.fromList $ fmap (\v -> (normWorld v, v)) (step ws)

{-
  The estimation is the total distance for every item to get to 4th floor.
  This would underestimate as:

  - it doesn't consider current level and unsafe states
  - despite that we can move at most 2 items at a time,
    1 item is needed for elevator to function,
    so effectively we still carry items one by one.

  Also that this esimation has the property that we are in a goal state
  iff. estimation says 0.
 -}
estimateDist :: WorldState -> Int
estimateDist (_, floors) =
  sum $ zipWith (\dist fs -> dist * sum (fmap length $ IM.elems fs)) [3, 2, 1] floors

aStar :: PQ.PSQ WorldState (Arg Int Int) -> M.Map WorldStateNorm Int -> Int
aStar q0 dists = case PQ.minView q0 of
  Nothing -> error "queue exhausted"
  Just (ws PQ.:-> (Arg fScore dist), q1) ->
    if fScore == dist
      then dist
      else
        let nexts = do
              p@(nws', ws') <- M.toList $ step' ws
              let mDists' = dists M.!? nws'
                  dist' = dist + 1
                  fScore' = dist' + estimateDist ws'
              guard $ maybe True (dist' <) mDists'
              pure (p, dist', Arg fScore' dist')
            q2 = foldr upd q1 nexts
              where
                upd ((_, ws'), _, prio') = PQ.insert ws' prio'
            dists' = foldr upd dists nexts
              where
                upd ((nws', _), dist', _) = M.insert nws' dist'
         in aStar q2 dists'

solve :: ParsedInput -> Int
solve inp =
  aStar
    (PQ.singleton initSt (Arg (estimateDist initSt) 0))
    (M.singleton (normWorld initSt) 0)
  where
    objTypes = [t | fl <- inp, (t, _) <- fl]
    {-
      turn element string into ints, so we can use IntMap and avoid having to
      carry those strings around in WorldState.
     -}
    (sToI, _iToS) = internalize objTypes
    initSt :: WorldState
    initSt =
      ( 0
      , fmap (IM.fromListWith (<>) . fmap (\(k, v) -> (sToI k, v NE.:| []))) inp
      )

instance Solution Day11 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (ex, rawInput) <- consumeExtra getInputS
    let (runPart1, runPart2) = shouldRun ex
        inp = consumeOrDie inputP rawInput
    when runPart1 do
      answerShow $ solve inp
    {-
      the example puzzle can't be used to run part2,
      as the 1st floor microchips would be fried immediately
      by newly introduced generators.
     -}
    when runPart2 do
      let inp2 =
            inp & ix 0
              %~ (<>
                    [ (e, o)
                    | e <- ["elerium", "dilithium"]
                    , o <- [Generator, Microchip]
                    ])
      answerShow $ solve inp2
