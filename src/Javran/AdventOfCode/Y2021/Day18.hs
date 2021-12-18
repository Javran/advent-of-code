{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2021.Day18
  (
  )
where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Function
import Data.Maybe
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, many)

data Day18 deriving (Generic)

data SfNum
  = SfReg Int
  | SfPair SfNum SfNum

{-
  Context stack, grow by prepending.

  - `Left r` represents context `SfPair <focus> r`
  - `Right l` represents context `SfPair l <focus>`
 -}
type SfZipperContext = [Either SfNum SfNum]

data SfZipper = Sfz
  { sfzFocus :: SfNum
  , sfzContext :: SfZipperContext
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

goLeft, goRight, goUp :: SfZipper -> Maybe SfZipper
goLeft z = do
  (Sfz (SfPair l r) ctxt) <- pure z
  pure (Sfz l (Left r : ctxt))
goRight z = do
  (Sfz (SfPair l r) ctxt) <- pure z
  pure (Sfz r (Right l : ctxt))
goUp z = do
  (Sfz x (hd : ctxt)) <- pure z
  pure $
    Sfz
      (case hd of
         Left r -> SfPair x r
         Right l -> SfPair l x)
      ctxt

addLeft, addRight :: Int -> SfZipperContext -> SfZipperContext
addLeft n ctxt = case ctxt of
  [] -> []
  hd@(Left _) : ctxt' -> hd : addLeft n ctxt'
  (Right l) : ctxt' -> Right (addToRightMost n l) : ctxt'
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

{-
  Recognizes immediate redex and performs reduction.
 -}
mustExplode, mustSplit :: SfZipper -> Maybe SfZipper
mustExplode z = do
  (Sfz (SfPair (SfReg l) (SfReg r)) ctxt@[_, _, _, _]) <- pure z
  pure $ Sfz (SfReg 0) (addRight r $ addLeft l ctxt)
mustSplit z = do
  (Sfz (SfReg n) ctxt) <- pure z
  guard $ n >= 10
  let hn = n `quot` 2
      (l, r) = if even n then (hn, hn) else (hn, hn + 1)
  pure $ Sfz (SfPair (SfReg l) (SfReg r)) ctxt

{-
  Lifts a visitor to visit everywhere.
 -}
visitZipper :: (SfZipper -> Maybe a) -> SfZipper -> Maybe a
visitZipper visitor = fix $ \performVisit z ->
  visitor z <|> case z of
    (Sfz SfPair {} _) ->
      (goLeft z >>= performVisit) <|> (goRight z >>= performVisit)
    (Sfz SfReg {} _) -> Nothing

fromZipper :: SfZipper -> SfNum
fromZipper z@(Sfz foc _) = maybe foc fromZipper (goUp z)

toZipper :: SfNum -> SfZipper
toZipper n = Sfz n []

reduceUntilFix :: SfNum -> SfNum
reduceUntilFix = fromJust . reduceUntilFix'
  where
    reduceWithOp op n =
      (fromZipper <$> op (toZipper n)) >>= reduceUntilFix'

    reduceUntilFix' :: SfNum -> Maybe SfNum
    reduceUntilFix' n =
      asum
        [ reduceWithOp (visitZipper mustExplode) n
        , reduceWithOp (visitZipper mustSplit) n
        , pure n
        ]

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
