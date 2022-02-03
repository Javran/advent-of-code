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

{-
  Note that this solution uses ByteString.Char8 instead of the usual String
  that we are working with.

  This has a significant speed boost and my original plan is to use tools that we built
  in part 1 to see how far can we go in part 2.

  That plan didsn't work - I'm not aware of any algorithm
  that can handle this kind of puzzles in general
  (that said, probably someone can do that with some forms of dynamic programming),
  so I ended up with something else taking advantage of properties that
  many websites' input data seems to share.

  This ByteString.Char8 impl remains however, just to have some examples
  that works with ByteString for once
  (note that for proper unicode handling, we need Data.Text of course).
 -}
solvePart1 :: [String] -> String -> Int
solvePart1 rawRules inp =
  S.size $ S.fromList $ performReplace rules inp'
  where
    inp' = BSC.pack inp
    rules :: Rules
    rules = M.toAscList $ M.fromListWith (<>) $ fmap tr rawRules
      where
        tr x =
          let [a, b] = splitOn " => " x
           in (BSC.pack a, [BSC.pack b])

{-
  Below are for part2.

  First thing, it might be easier to think about the medicine molecule as the input string,
  and work backwards, "unapplying" rules until it becomes just `e`.

  A general search doesn't work, however. As there are simply too many search states
  to explore - a small portion of the input string could have multiple ways of parsing
  and putting those portions together the search space will definitely explode.

  So my solution needs to exploit properties in my login input
  (and also some other login input samples to see what is common).

  Let's begin with some analysis on my login input:
  (as I've later found out that all examples have a fixed set of rules,
  just that the input strings differ)

  Among all those rules:

  - it seems all RHS are unique, in other words, there is no branching
    by "unapplying" a rules from RHS to LHS.
    (however, the order of un-application might matter)

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

    The significance of this is that we now know we must get those A, B, C
    to single atoms before we can apply any of those rules.
    so if a sequence of atoms appears in one of those A, B, C positions,
    we should only accept parsing that results in one single atom.

  - By playing around with some subsequences of the input string,
    I think most of them has a unique parsing result despite taking
    different paths - this further confirms that we can probably
    get away without exploring the full search space by taking
    the first few viable way of parsing.

 -}

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
  Output type explained:

  - Left (_ :: (Atom, Atom)): expect exactly two atoms on RHS
  - Right (_ :: Atom, _ :: RnAr):
    expect one atom followed by one of those RnAr chunks.

 -}
rule2P
  :: ReadP
       ( Atom
       , Either (Atom, Atom) (Atom, RnAr Atom)
       )
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

{-
  Exactly the same function as splitAts with
  a different type - we could genrealize this
  with a typeclass but I don't want the complexity of going that far.
 -}
splitAts2 :: [Inp] -> [] ([Inp], [Inp])
splitAts2 xs = do
  n <- [0 .. length xs]
  pure $ splitAt n xs

type Rules2 = M.Map [Inp] Atom

{-
  Very similar to performReplace but for part2
  we have only one RHS (was originally a LHS for a rule) to worry about.
 -}
performReplace2 :: Rules2 -> [Inp] -> [] [Inp]
performReplace2 rules inp = do
  (pre, xs0) <- splitAts2 inp
  (lhs, rhs) <- M.toList rules
  Just xs1 <- pure $ stripPrefix lhs xs0
  pure $ pre <> [Flat rhs] <> xs1

{-
  Simplifying is parsing, keeping track of rule application count.
 -}
simpInp :: Rules2 -> Inp -> WriterT (Sum Int) [] Inp
simpInp rs = \case
  x@Flat {} -> pure x
  Nest rnAr -> Nest <$> mapM simp' rnAr
  where
    simp' i = do
      [o] <- simp rs i
      pure [o]

{-
  Keeps applying a set of rules until nothing further can be applied,
  this search is prioritized on length of the current string
  (shorter one in queue is always checked first) to produce
  a stream of final parsing results, roughly ordered by length.
  (it probably won't be a sorted list, as a shorter way of parsing
  might appear late in the search that a longer one has been in
  the result list prior to that).
 -}
replaceUntilFix
  :: Rules2
  -> S.Set [Inp]
  -> PQ.PSQ [Inp] (Arg Int Int)
  -> [([Inp], Int)]
replaceUntilFix rules = fix \go discovered q0 -> case PQ.minView q0 of
  Nothing -> []
  Just (a PQ.:-> Arg _l cnt, q1) ->
    let nexts = performReplace2 rules a
     in if null nexts
          then (a, cnt) : go discovered q1
          else
            let nexts' = do
                  next <- nexts
                  guard $ S.notMember next discovered
                  pure (next, length next, cnt + 1)
                q2 = foldr (\(next, l', cnt') -> PQ.insert next (Arg l' cnt')) q1 nexts'
                discovered' = foldr (\(n, _, _) -> S.insert n) discovered nexts'
             in go discovered' q2

{-
  simp and simpInp are mutually recursive to work on
  individual elements and then the sequence as a whole.
 -}
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

solvePart2 :: [String] -> String -> Int
solvePart2 rawRules inp = ans2
  where
    rules :: Rules2
    rules = M.fromListWith (error "duplicated key") do
      rawRule <- rawRules
      let (oLhs, oRhs) = consumeOrDie rule2P rawRule
          lhs' = case oRhs of
            Left (a, b) -> [Flat a, Flat b]
            Right (a, ra) -> [Flat a, Nest $ fmap ((: []) . Flat) ra]
      pure (lhs', oLhs)
    inp2 = consumeOrDie (many inpP) inp
    (_, Sum ans2) : _ = runWriterT $ simp rules inp2

instance Solution Day19 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (ex, rawInput) <- consumeExtra getInputS
    let [rawRules, [inp]] = splitOn [""] . lines $ rawInput
        (runPart1, runPart2) = shouldRun ex
    when runPart1 do
      answerShow $ solvePart1 rawRules inp
    when runPart2 do
      answerShow $ solvePart2 rawRules inp
