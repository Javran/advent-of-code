{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Javran.AdventOfCode.Y2015.Day19
  (
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer.CPS
import qualified Data.ByteString.Char8 as BSC
import Data.Char
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import qualified Data.PSQueue as PQ
import Data.Semigroup
import qualified Data.Set as S
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.TestExtra
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day19 deriving (Generic)

type Str = BSC.ByteString

{-
  Splits the list at all possible positions.

  e.g.:
  > splitAts "abcde"
  [("","abcde"),("a","bcde"),("ab","cde"),("abc","de"),("abcd","e"),("abcde","")]

 -}
splitAts :: Str -> [] (Str, Str)
splitAts xs = do
  n <- [0 .. BSC.length xs]
  pure $ BSC.splitAt n xs

performReplace :: Rules -> Str -> [] Str
performReplace rules inp = do
  (pre, xs0) <- splitAts inp
  (lhs, rhss) <- rules
  Just xs1 <- pure $ BSC.stripPrefix lhs xs0
  rhs <- rhss
  pure $ pre <> rhs <> xs1

type Rules = [(Str, [Str])]

type Atom = String

data RnAr a
  = Y0 a
  | Y1 a a
  | Y2 a a a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data Inp
  = Flat Atom
  | Nest (RnAr [Inp])
  deriving (Show, Eq, Ord)

atomP :: ReadP Atom
atomP = do
  atom <- (:) <$> satisfy isAsciiUpper <*> munch isAsciiLower
  guard $ atom `notElem` ["Rn", "Y", "Ar"]
  pure atom

inpP :: ReadP Inp
inpP = rnArP <++ (Flat <$> atomP)
  where
    rnArP = between (strP "Rn") (strP "Ar") do
      xs <- many1 inpP `sepBy1` char 'Y'
      pure $ Nest case xs of
        [a] -> Y0 a
        [a, b] -> Y1 a b
        ~[a, b, c] -> Y2 a b c

{-
  TODO:
  Some analysis on my login input, which might or might not be useful:

  - only on rule's LHS: ["e"]

  - only on rule's RHS: ["Ar","C","Rn","Y"]

    + further, `Rn` and `Ar` always appear in pairs, in that order,
      and `Y` only appears between them.

 - rules follow one of the following forms:

   + X => Y Z
     where X, Y, and Z are atoms

   + X => Y `Rn` A `Ar`
       or Y `Rn` A `Y` B `Ar`
       or Y `Rn` A `Y` B `Y` C `Ar`
     where X, Y, Z, A, B, and C are atoms.

  - sequence seems to have unique parsing,
    or at least if we prioritize inside a nested structure,
    alternatives won't be much.

  - For RnAr, we probably can get all fields of Y0 / Y1 / Y2
    to a single atom.

  TODO: a bit messy, cleanup needed.

 -}

rule2P :: ReadP (Atom, Either (Atom, Atom) (Atom, RnAr Atom))
rule2P = do
  lhs <- atomP <++ ("e" <$ char 'e')
  strP " => "
  rhsPre <- many1 inpP
  rhs <- case rhsPre of
    [Flat a, Flat b] -> pure $ Left (a, b)
    [Flat a, Nest xs] ->
      (\v -> Right (a, v))
        <$> (case xs of
               Y0 [Flat x0] -> pure $ Y0 x0
               Y1 [Flat x0] [Flat x1] -> pure $ Y1 x0 x1
               Y2 [Flat x0] [Flat x1] [Flat x2] -> pure $ Y2 x0 x1 x2
               _ -> pfail)
    _ -> pfail
  pure (lhs, rhs)

splitAts2 :: [Inp] -> [] ([Inp], [Inp])
splitAts2 xs = do
  n <- [0 .. length xs]
  pure $ splitAt n xs

type Rules2 = M.Map [Inp] Atom

performReplace2 :: Rules2 -> [Inp] -> [] [Inp]
performReplace2 rules inp = do
  (pre, xs0) <- splitAts2 inp
  (lhs, rhs) <- M.toList rules
  Just xs1 <- pure $ stripPrefix lhs xs0
  pure $ pre <> [Flat rhs] <> xs1

simpInp :: Rules2 -> Inp -> WriterT (Sum Int) [] Inp
simpInp rs = \case
  x@Flat {} -> pure x
  Nest rnAr -> Nest <$> mapM simp' rnAr
  where
    simp' i = do
      [o] <- simp rs i
      pure [o]

replaceUntilFix
  :: Rules2
  -> S.Set [Inp]
  -> PQ.PSQ [Inp] (Arg Int Int)
  -> [([Inp], Int)]
replaceUntilFix rules = fix \go discovered q0 -> case PQ.minView q0 of
  Nothing -> []
  Just (a PQ.:-> Arg _l cnt, q1) ->
    let nexts = performReplace2 rules a
     in case nexts of
          [] -> (a, cnt) : go discovered q1
          _ : _ ->
            let nexts' = do
                  next <- nexts
                  guard $ S.notMember next discovered
                  pure (next, length next, cnt + 1)
                q2 = foldr (\(next, l', cnt') -> PQ.insert next (Arg l' cnt')) q1 nexts'
                discovered' = foldr (\(n, _, _) -> S.insert n) discovered nexts'
             in go discovered' q2

simp :: Rules2 -> [Inp] -> WriterT (Sum Int) [] [Inp]
simp rs xs = do
  xs' <- mapM (simpInp rs) xs
  (r, l) <-
    lift $
      replaceUntilFix
        rs
        (S.singleton xs')
        (PQ.singleton xs' $ Arg (length xs') 0)
  tell (Sum l)
  pure r

instance Solution Day19 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (ex, rawInput) <- consumeExtra getInputS
    let [rawRules, [inp]] = splitOn [""] . lines $ rawInput
        inp' = BSC.pack inp
        (runPart1, runPart2) = shouldRun ex
    when runPart1 do
      let rules = M.toAscList $ M.fromListWith (<>) $ fmap tr rawRules
            where
              tr x =
                let [a, b] = splitOn " => " x
                 in (BSC.pack a, [BSC.pack b])
      answerShow (S.size $ S.fromList $ performReplace rules inp')
    when runPart2 do
      let rules :: Rules2
          rules = M.fromListWith (error "duplicated key") do
            rawRule <- rawRules
            let (oLhs, oRhs) = consumeOrDie rule2P rawRule
                lhs' = case oRhs of
                  Left (a, b) -> [Flat a, Flat b]
                  Right (a, ra) -> [Flat a, Nest $ fmap ((: []) . Flat) ra]
            pure (lhs', oLhs)
          inp2 = consumeOrDie (many inpP) inp
          (_, Sum ans2) : _ = runWriterT $ simp rules inp2
      answerShow ans2
