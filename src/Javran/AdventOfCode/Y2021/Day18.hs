{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2021.Day18
  (
  )
where

import Control.Applicative
import Data.Maybe
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, many)

data Day18 deriving (Generic)

data SfNum
  = SfReg Int
  | SfPair SfNum SfNum

data SfZipper = Sfz
  { sfzFocus :: SfNum
  , sfzContext :: [Either SfNum SfNum] -- choose left part or choose right part
  }
  deriving (Show)

instance Show SfNum where
  show = \case
    SfReg n -> show n
    SfPair a b -> '[' : show a <> "," <> show b <> "]"

sfNumP :: ReadP SfNum
sfNumP =
  (SfReg <$> decimal1P)
    <++ (char '[' *> (SfPair <$> (sfNumP <* char ',') <*> sfNumP) <* char ']')

goLeft :: SfZipper -> Maybe SfZipper
goLeft (Sfz (SfPair l r) ctxt) = Just (Sfz l (Left r : ctxt))
goLeft (Sfz SfReg {} _) = Nothing

goRight :: SfZipper -> Maybe SfZipper
goRight (Sfz (SfPair l r) ctxt) = Just (Sfz r (Right l : ctxt))
goRight (Sfz SfReg {} _) = Nothing

sfUnroll :: SfZipper -> Maybe SfZipper
sfUnroll (Sfz _ []) = Nothing
sfUnroll (Sfz x (hd : ctxt)) = case hd of
  Left r -> Just (Sfz (SfPair x r) ctxt)
  Right l -> Just (Sfz (SfPair l x) ctxt)

addLeft :: Int -> [Either SfNum SfNum] -> [Either SfNum SfNum]
addLeft n ctxt = case ctxt of
  [] -> []
  hd@(Left _) : ctxt' -> hd : addLeft n ctxt'
  (Right l) : ctxt' -> Right (addToRightMost n l) : ctxt'

addRight :: Int -> [Either SfNum SfNum] -> [Either SfNum SfNum]
addRight n ctxt = case ctxt of
  [] -> []
  hd@(Right _) : ctxt' -> hd : addRight n ctxt'
  (Left r) : ctxt' -> Left (addToLeftMost n r) : ctxt'

addToLeftMost, addToRightMost :: Int -> SfNum -> SfNum
addToLeftMost n = \case
  SfReg v -> SfReg (v + n)
  SfPair l r -> SfPair (addToLeftMost n l) r
addToRightMost n = \case
  SfReg v -> SfReg (v + n)
  SfPair l r -> SfPair l (addToRightMost n r)

tryExplode :: SfZipper -> Maybe SfZipper
tryExplode z = case z of
  (Sfz (SfPair (SfReg l) (SfReg r)) ctxt@[_, _, _, _]) ->
    Just $ Sfz (SfReg 0) (addRight r $ addLeft l ctxt)
  (Sfz SfPair {} _) ->
    (goLeft z >>= tryExplode) <|> (goRight z >>= tryExplode)
  (Sfz SfReg {} _) -> Nothing

trySplit :: SfZipper -> Maybe SfZipper
trySplit z = case z of
  (Sfz (SfReg n) ctxt)
    | n >= 10 ->
      let hn = n `quot` 2
          (l, r) = if even n then (hn, hn) else (hn, hn + 1)
       in pure $ Sfz (SfPair (SfReg l) (SfReg r)) ctxt
  (Sfz (SfReg _) _) -> Nothing
  (Sfz SfPair {} _) ->
    (goLeft z >>= trySplit) <|> (goRight z >>= trySplit)

sfUnrollAll :: SfZipper -> SfNum
sfUnrollAll z@(Sfz foc _) = maybe foc sfUnrollAll (sfUnroll z)

toZipper :: SfNum -> SfZipper
toZipper n = Sfz n []

reduceUntilFix :: SfNum -> SfNum
reduceUntilFix = fromJust . reduceUntilFix'
  where
    reduceUntilFix' :: SfNum -> Maybe SfNum
    reduceUntilFix' n =
      (do
         z' <- tryExplode (toZipper n)
         reduceUntilFix' (sfUnrollAll z'))
        <|> (do
               -- explode failed, try split.
               z' <- trySplit (toZipper n)
               reduceUntilFix' (sfUnrollAll z'))
        <|> pure n

sfAdd :: SfNum -> SfNum -> SfNum
sfAdd l r =
  -- assuming inputs are already reduced.
  reduceUntilFix $ SfPair l r

magnitude :: SfNum -> Int
magnitude = \case
  SfReg n -> n
  SfPair l r -> magnitude l * 3 + magnitude r * 2

instance Solution Day18 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (consumeOrDie sfNumP) . lines <$> getInputS
    answerShow $ magnitude $ foldl1 sfAdd xs
    answerShow $ maximum do
      (x, xs1) <- pick xs
      (y, _) <- pick xs1
      pure (magnitude (sfAdd x y))
