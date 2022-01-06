{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Javran.AdventOfCode.Y2018.Day16
  (
  )
where

import Control.Applicative
import Control.Lens hiding (universe)
import Control.Monad
import Data.Bits
import qualified Data.IntMap.Strict as IM
import Data.List
import Data.Monoid
import qualified Data.Set as S
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day16 deriving (Generic)

type Tuple4 a = (a, a, a, a)

data Sample = Sample
  { sBefore :: Tuple4 Int
  , sCode :: Tuple4 Int
  , sAfter :: Tuple4 Int
  }
  deriving (Show)

inputP :: ReadP ([Sample], [Tuple4 Int])
inputP =
  (,)
    <$> manyTill (sampleP <* char '\n') (string "\n\n")
    <*> many tuple4nl
  where
    list4nl = between (char '[') (string "]\n") do
      ~[a, b, c] <- replicateM 3 (decimal1P <* string ", ")
      d <- decimal1P
      pure (a, b, c, d)
    tuple4nl = do
      [a, b, c, d] <- decimal1P `sepBy1` char ' '
      (a, b, c, d) <$ char '\n'

    sampleP :: ReadP Sample
    sampleP =
      Sample <$> (string "Before: " *> list4nl)
        <*> tuple4nl
        <*> (string "After:  " *> list4nl)

data Register = R0 | R1 | R2 | R3 deriving (Enum)

data ValueMode = Reg | Imm deriving (Enum, Bounded, Show, Ord, Eq)

{-
  Looks a bit ugly, but this is probably best we can do without too much machinery:

  https://stackoverflow.com/q/70590623/315302
 -}
data BinValueMode
  = ImmReg
  | RegImm
  | RegReg
  deriving (Enum, Bounded, Show, Ord, Eq)

data OpType
  = Add ValueMode
  | Mul ValueMode
  | BitAnd ValueMode
  | BitOr ValueMode
  | Assign ValueMode
  | TestGreaterThan BinValueMode
  | TestEqual BinValueMode
  deriving (Show, Ord, Eq)

type DeviceState = Tuple4 Int

allOpTypes :: [OpType]
allOpTypes =
  ([Add, Mul, BitAnd, BitOr, Assign] <*> universe)
    <> ([TestGreaterThan, TestEqual] <*> universe)

interpret :: DeviceState -> OpType -> (Int, Int, Int) -> Maybe DeviceState
interpret ds opType (a, b, c) = case opType of
  Add mb -> cat0 mb (+)
  Mul mb -> cat0 mb (*)
  BitAnd mb -> cat0 mb (.&.)
  BitOr mb -> cat0 mb (.|.)
  Assign ma -> do
    v0 <- getVal ma a
    rOut <- resolveReg c
    pure $ ds & _r rOut .~ v0
  TestGreaterThan mab -> cat1 mab (>)
  TestEqual mab -> cat1 mab (==)
  where
    -- category0 covers ops with prefix add / mul / ban / bor
    cat0 mb f = do
      v0 <- getVal Reg a
      v1 <- getVal mb b
      rOut <- resolveReg c
      pure $ ds & _r rOut .~ f v0 v1
    -- category1 covers ops with prefix gt / eq
    cat1 mab f = do
      let (ma, mb) = case mab of
            ImmReg -> (Imm, Reg)
            RegImm -> (Reg, Imm)
            RegReg -> (Reg, Reg)
      v0 <- getVal ma a
      v1 <- getVal mb b
      rOut <- resolveReg c
      pure $ ds & _r rOut .~ bool 0 1 (f v0 v1)
    _r = \case
      R0 -> _1
      R1 -> _2
      R2 -> _3
      R3 -> _4

    getVal vm i = case vm of
      Reg -> do
        r <- resolveReg i
        pure $ ds ^. _r r
      Imm -> pure i

    resolveReg :: Int -> Maybe Register
    resolveReg i =
      toEnum @Register i <$ guard (i >= 0 && i <= 3)

interpret2 :: IM.IntMap OpType -> Tuple4 Int -> DeviceState -> DeviceState
interpret2 opTable (opCode, a, b, c) ds =
  fromJust $ interpret ds (opTable IM.! opCode) (a, b, c)

isConsistent :: OpType -> Sample -> Bool
isConsistent opTyp Sample {sBefore, sCode = (_, b, c, d), sAfter} = isJust do
  actualAfter <- interpret sBefore opTyp (b, c, d)
  guard $ sAfter == actualAfter

allConsistentOpTypes :: Sample -> [OpType]
allConsistentOpTypes e = filter (`isConsistent` e) allOpTypes

{-
  TODO: I feel this type of search comes up a bit frequent,
  should we generalize this?
 -}
solve :: IM.IntMap (S.Set OpType) -> IM.IntMap OpType -> [IM.IntMap OpType]
solve clues assigned =
  if null clues
    then pure assigned
    else do
      let (opCode, alts) : _ = sortOn (length . snd) $ IM.toList clues
      opType <- S.toList alts
      let clues' = IM.map (S.delete opType) $ IM.delete opCode clues
      guard $ not (any null clues')
      solve clues' (IM.insert opCode opType assigned)

instance Solution Day16 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    (samples, code) <- consumeOrDie inputP <$> getInputS
    let individualClues :: [(Int, [OpType])]
        individualClues =
          fmap
            (\s@Sample {sCode = (opCode, _, _, _)} ->
               (opCode, allConsistentOpTypes s))
            samples
    answerShow $
      countLength
        (\(_, ops) -> case ops of
           _ : _ : _ : _ -> True
           _ -> False)
        individualClues
    do
      let clues = IM.fromListWith S.intersection do
            (opCode, opTypes) <- individualClues
            pure (opCode, S.fromList opTypes)
          -- expect a unique solution,
          -- otherwise it's likely that we don't have a unique answer for part 2.
          [opCodeTable] = solve clues IM.empty
          prog =
            appEndo
              . getDual
              . foldMap (Dual . Endo . interpret2 opCodeTable)
              $ code
          (ans, _, _, _) = prog (0, 0, 0, 0)
      answerShow ans
