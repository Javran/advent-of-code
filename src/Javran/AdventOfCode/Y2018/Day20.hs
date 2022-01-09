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

module Javran.AdventOfCode.Y2018.Day20
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Data.Char
import Data.Containers.ListUtils
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Monoid
import qualified Data.PSQueue as PQ
import Data.Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Shower
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

{-
  smart constructor to make sure we never directly nest ReSeq.
 -}
mkReSeq :: [Re] -> Re
mkReSeq =
  ReSeq . concatMap \case
    ReSeq xs -> xs
    re -> [re]

headTailReAux :: Re -> [(Dir, Re)]
headTailReAux = \case
  ReAtom d -> pure (d, ReSeq [])
  ReSeq [] -> []
  ReSeq (hd : tl) -> do
    (d, hd') <- headTailReAux hd
    pure (d, mkReSeq (hd' : tl))
  ReAlt xs -> xs >>= headTailReAux

headTailRe :: Re -> [(Dir, Re)]
headTailRe re = fmap (second ReAlt) $ M.toList m
  where
    m = M.fromListWith (<>) do
      (d, alt) <- headTailReAux re
      pure (d, [alt])

hdTl :: StateT Re [] Dir
hdTl = do
  re <- get
  (d, re') <- lift $ headTailRe re
  d <$ put re'

tryDir :: Dir -> StateT Re [] Dir
tryDir d = do
  re <- get
  let alts = filter ((== d) . fst) $ headTailRe re
  (_, re') <- lift alts
  put re'
  pure d

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
    reSeqP = mkReSeq <$> (many (between (char '(') (char ')') reAltP <++ reAtomP))

    reAtomP :: ReadP Re
    reAtomP =
      foldl1
        (<++)
        [ ReAtom N <$ char 'N'
        , ReAtom W <$ char 'W'
        , ReAtom S <$ char 'S'
        , ReAtom E <$ char 'E'
        ]

type Nfa = IM.IntMap (M.Map (Maybe Dir) IS.IntSet)

freshState :: State (Int, Nfa) Int
freshState = state (\(v, nfa) -> (v, (v + 1, nfa)))

linkState :: Int -> Maybe Dir -> Int -> State (Int, Nfa) ()
linkState u ch v =
  modify
    (second $
       IM.alter
         (\case
            Nothing -> Just $ M.singleton ch (IS.singleton v)
            Just m ->
              Just $
                M.alter
                  (\case
                     Nothing -> Just (IS.singleton v)
                     Just vs -> Just (IS.insert v vs))
                  ch
                  m)
         u)

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
match nfa starts dir = findEps do
  start <- IS.toList starts
  Just ends <- pure $ (nfa IM.!? start) >>= (M.!? (Just dir))
  IS.toList ends
  where
    findEps :: [Int] -> IS.IntSet
    findEps xs = findEpsAux (IS.fromList xs) (Seq.fromList xs)

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

buildGraph :: Nfa -> Graph -> PQ.PSQ Coord (Arg () IS.IntSet) -> Graph
buildGraph nfa g q0 = case PQ.minView q0 of
  Nothing -> g
  Just ((coord PQ.:-> Arg () st), q1) ->
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

{-
  TODO: since we have a very large login input,
  we probably don't want to explore all inputs that the RE can recognize.
  Instead, let's try pushing REs to individual cells and expand from there.
  This will allow those that takes different paths to get to the same coord
  to be handled at the same time.

  TODO: it's probably difficult to get epsilon-handling right,
  so let's build NFA instead.

 -}
instance Solution Day20 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (extraOps, rawInput) <- consumeExtraLeadingLines <$> getInputS
    let re = consumeOrDie reP . head . lines $ rawInput
        (_, nfa) = execState (buildNfa 0 re 1) (2, IM.empty)
        g :: Graph
        g = buildGraph nfa M.empty (PQ.singleton (0, 0) (Arg () (IS.singleton 0)))
        Just (MinMax2D ((rMin, rMax), (cMin, cMax))) = foldMap (Just . minMax2D) $ M.keys g
        part2Limit = case extraOps of
          Just raw -> read (head raw)
          Nothing -> 1000
    forM_ [rMin .. rMax] \r -> do
      let render2 c =
            ( "#" <> (if up then "-" else "#")
            , (if left then "|" else "#") <> (if (r, c) == (0, 0) then "X" else ".")
            )
            where
              up = ((g M.!? (r, c)) >>= pure . S.member (r -1, c)) == Just True
              left = ((g M.!? (r, c)) >>= pure . S.member (r, c -1)) == Just True
          (l1, l2) = unzip $ fmap render2 [cMin .. cMax]
      putStrLn $ (concat l1) <> "#"
      putStrLn $ (concat l2) <> "#"
    putStrLn $ replicate (1 + 2 * (cMax - cMin + 1)) '#'
    let shortestDists = runSpfa g (0,0)
    answerShow $ maximum shortestDists
    answerShow $ M.size $ M.filter (>= part2Limit) shortestDists
