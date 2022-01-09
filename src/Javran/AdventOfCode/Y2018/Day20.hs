{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Javran.AdventOfCode.Y2018.Day20
  (
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Data.Functor
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import Data.Monoid
import qualified Data.PSQueue as PQ
import Data.Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day20 deriving (Generic)

data Dir = N | W | S | E deriving (Show, Eq, Ord)

type Coord = (Int, Int) -- row, col

applyDir :: Dir -> Coord -> Coord
applyDir = \case
  N -> first pred
  S -> first succ
  W -> second pred
  E -> second succ

data Re
  = ReAtom Dir
  | ReSeq [Re]
  | ReAlt [Re]
  deriving (Show)

reP :: ReadP Re
reP = between (char '^') (char '$') reAltP
  where
    reAltP :: ReadP Re
    reAltP = ReAlt <$> (reSeqP `sepBy` char '|')

    {- Parses a regualar expr free of direct `|`s -}
    reSeqP :: ReadP Re
    reSeqP = ReSeq <$> many (between (char '(') (char ')') reAltP <++ reAtomP)

    reAtomP :: ReadP Re
    reAtomP =
      foldl1
        (<++)
        [ ReAtom N <$ char 'N'
        , ReAtom W <$ char 'W'
        , ReAtom S <$ char 'S'
        , ReAtom E <$ char 'E'
        ]

{-
  See: https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton

  Matching directly with RegEx is tricky especially when dealing with
  nested structures of ReSeq and ReAlt. So instead of direct handling,
  we build up the equivalent NFA to perform matching.
 -}
type Nfa = IM.IntMap (M.Map (Maybe Dir) IS.IntSet)

freshState :: State (Int, Nfa) Int
freshState = state (\(v, nfa) -> (v, (v + 1, nfa)))

linkState :: Int -> Maybe Dir -> Int -> State (Int, Nfa) ()
linkState u ch v =
  modify
    (second $
       IM.insertWith
         (M.unionWith IS.union)
         u
         (M.singleton ch (IS.singleton v)))

buildNfa :: Int -> Re -> Int -> State (Int, Nfa) ()
buildNfa start re end = case re of
  ReAtom dir ->
    linkState start (Just dir) end
  ReSeq [] ->
    linkState start Nothing end
  ReSeq xs -> do
    ys <- replicateM (length xs -1) freshState
    let zipped = zip (zip (start : ys) (ys <> [end])) xs
    forM_ zipped \((u, v), re') ->
      buildNfa u re' v
  ReAlt xs ->
    forM_ xs \re' ->
      buildNfa start re' end

match :: Nfa -> IS.IntSet -> Dir -> IS.IntSet
match nfa starts dir = epsilonClosure nfa do
  start <- IS.toList starts
  Just ends <- pure $ (nfa IM.!? start) >>= (M.!? Just dir)
  IS.toList ends

epsilonClosure :: Nfa -> [Int] -> IS.IntSet
epsilonClosure nfa xs = findEpsAux (IS.fromList xs) (Seq.fromList xs)
  where
    findEpsAux seen = \case
      Seq.Empty -> seen
      u Seq.:<| q1 ->
        let nexts = do
              Just vs <- pure $ nfa IM.!? u >>= (M.!? Nothing)
              v <- IS.toList vs
              guard $ IS.notMember v seen
              pure v
            seen' = foldr IS.insert seen nexts
         in findEpsAux seen' $ q1 <> Seq.fromList nexts

type Graph = M.Map Coord (S.Set Coord)

insertGraph :: Coord -> Coord -> Graph -> Graph
insertGraph a b =
  M.insertWith S.union b (S.singleton a)
    . M.insertWith S.union a (S.singleton b)

{-
  Simply following the NFA works, but it's inefficient.
  This is because, for this 2d-map setup, it's very likely that two different paths
  with take us to a exact same location, and we have to explore the same location
  over and over again for exploring different part of the RegEx.

  So instead of doing that, we keep track of a mapping from coordinates to
  the set of NFA states that we are in, benefits are:

  - this constraints the size of the queue to the # of coordinates in the map
  - when the same coord is reached from different paths, we can join NFA states
    together, so to increase "parallelism".

  Note that we are not really using the priority here - we simply use PSQ
  as a structure to hold our todo-list.
 -}
buildGraph :: Nfa -> Graph -> PQ.PSQ Coord (Arg () IS.IntSet) -> Graph
buildGraph nfa g q0 = case PQ.minView q0 of
  Nothing -> g
  Just (coord PQ.:-> Arg () st, q1) ->
    {-
      The queue is our todo list:
      for now we are looking at cordinate `coord`,
      which is at NFA states `st`,
      we try all directions and use `match` to get next set of states `st'`.
     -}
    let nexts = do
          dir <- [N, W, S, E]
          let coord' = applyDir dir coord
              st' = match nfa st dir
          guard $ not (IS.null st')
          pure (coord', st')
        g' =
          foldr
            (\(coord', _) -> insertGraph coord coord')
            g
            nexts
        q2 = foldr upd q1 nexts
          where
            upd (coord', st') =
              PQ.alter
                (\case
                   Nothing -> Just (Arg () st')
                   Just (Arg () s) -> Just (Arg () (IS.union s st')))
                coord'
     in buildGraph nfa g' q2

runSpfa :: Graph -> Coord -> M.Map Coord Int
runSpfa g start = execState (spfaWith (PQ.singleton start 0)) (M.singleton start 0)
  where
    spfaWith q0 =
      case PQ.minView q0 of
        Nothing -> pure ()
        Just (u PQ.:-> distU, q1) -> do
          performEnqs <- forM (S.toList (g M.! u)) $ \v -> do
            let distV' = distU + 1
            mDistV <- gets (M.!? v)
            if maybe True (distV' <) mDistV
              then do
                modify (M.insert v distV')
                pure
                  (PQ.alter
                     (\case
                        Nothing -> Just distV'
                        Just distV -> Just (min distV distV'))
                     v)
              else pure id
          spfaWith $ appEndo (foldMap Endo performEnqs) q1

pprGraph :: Graph -> IO ()
pprGraph g = do
  let Just (MinMax2D ((rMin, rMax), (cMin, cMax))) = foldMap (Just . minMax2D) $ M.keys g
  forM_ [rMin .. rMax] \r -> do
    let render2 c =
          ( "#" <> if up then "-" else "#"
          , (if left then "|" else "#") <> if (r, c) == (0, 0) then "X" else "."
          )
          where
            up = ((g M.!? (r, c)) <&> S.member (r -1, c)) == Just True
            left = ((g M.!? (r, c)) <&> S.member (r, c -1)) == Just True
        (l1, l2) = unzip $ fmap render2 [cMin .. cMax]
    putStrLn $ concat l1 <> "#"
    putStrLn $ concat l2 <> "#"
  putStrLn $ replicate (1 + 2 * (cMax - cMin + 1)) '#'

instance Solution Day20 where
  solutionRun _ SolutionContext {getInputS, answerShow, terminal} = do
    (extraOps, rawInput) <- consumeExtraLeadingLines <$> getInputS
    let re = consumeOrDie reP . head . lines $ rawInput
        (_, nfa) = execState (buildNfa 0 re 1) (2, IM.empty)
        starts = epsilonClosure nfa [0]
        g :: Graph
        g = buildGraph nfa M.empty (PQ.singleton (0, 0) (Arg () starts))
        part2Limit = case extraOps of
          Just raw -> read (head raw)
          Nothing -> 1000
    when (isJust terminal) do
      pprGraph g
    let shortestDists = runSpfa g (0, 0)
        Just
          ( Max ans1
            , Sum ans2
            ) =
            foldMap
              (\d ->
                 Just
                   ( Max d
                   , if d >= part2Limit then 1 :: Sum Int else 0
                   ))
              shortestDists
    answerShow ans1
    answerShow ans2
