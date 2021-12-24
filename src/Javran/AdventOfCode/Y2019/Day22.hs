{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2019.Day22
  (
  )
where

import Data.List
import Data.Mod
import Data.Proxy
import Data.Semigroup
import GHC.Generics (Generic)
import GHC.TypeNats hiding (Mod)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, many)

data Day22 deriving (Generic)

data ShufTech
  = StDealIntoNewStack
  | StCutCards Int
  | StDealWithIncrement Int
  deriving (Show)

shufTechP :: ReadP ShufTech
shufTechP =
  (StCutCards <$> (string "cut " *> readS_to_P (reads @Int)))
    <++ do
      _ <- string "deal "
      (StDealIntoNewStack <$ string "into new stack")
        <++ (StDealWithIncrement <$> (string "with increment " *> decimal1P))

{-
  LinFn (a,b) is linear function under modulo m:

  f(x) = (a * x + b) `mod` m.

  For the index i after transformation f,
  f(i) is its original index.

 -}
newtype LinFn (m :: Nat) = LinFn (Mod m, Mod m) deriving (Show)

applyLinFn :: KnownNat m => LinFn m -> Mod m -> Mod m
applyLinFn (LinFn (a, b)) x = a * x + b

{-
  y = a*x + b
  => x = y * inv(a) - b * inv(a)
 -}
undoLinFn :: KnownNat m => LinFn m -> LinFn m
undoLinFn (LinFn (a, b)) = LinFn (invA, - invA * b)
  where
    Just invA = invertMod a

{-
  Given f(x) = a * x + b, g(x) = c * x + d,
  we have:

  g(f(x))
  = c * (a * x + b) + d
  = c * a * x + (c * b + d)

  Note that the arguments are (g `composeFn` f),

  as LinFn is composed in the opposite direction of how we apply ShufTech in order.
 -}
composeLinFn :: KnownNat m => LinFn m -> LinFn m -> LinFn m
LinFn (c, d) `composeLinFn` LinFn (a, b) = LinFn (c * a, c * b + d)

instance KnownNat m => Semigroup (LinFn m) where
  (<>) = composeLinFn

shufTechToLinFn :: ShufTech -> (forall m. KnownNat m => LinFn m)
shufTechToLinFn = \case
  StDealIntoNewStack -> LinFn (-1, -1)
  StCutCards n -> LinFn (1, fromIntegral n)
  StDealWithIncrement n ->
    let Just nInv = invertMod (fromIntegral n)
     in LinFn (nInv, 0)

instance Solution Day22 where
  solutionRun _ SolutionContext {getInputS, answerShow, answerS} = do
    (extraOps, rawInput) <- consumeExtraLeadingLines <$> getInputS
    let xs :: [ShufTech]
        xs = fmap (consumeOrDie shufTechP) . lines $ rawInput
        composed :: forall m. KnownNat m => LinFn m
        composed = foldl' (composeLinFn @m) (LinFn (1, 0)) (fmap shufTechToLinFn xs)
    case extraOps of
      Nothing -> do
        case someNatVal 10007 of
          SomeNat (_ :: Proxy m) -> do
            answerShow (unMod $ applyLinFn (undoLinFn (composed @m)) 2019)
        case someNatVal 119315717514047 of
          SomeNat (_ :: Proxy m) -> do
            -- TODO: test coverage for part 2.
            let tr = stimes @_ @Int 101741582076661 (composed @m)
            answerShow (unMod $ applyLinFn tr 2020)
      Just extra ->
        let p = read @Int (head extra)
         in case someNatVal (fromIntegral p) of
              SomeNat (_ :: Proxy m) -> do
                let steps =
                      tail $ scanl (composeLinFn @m) (LinFn (1, 0)) (fmap shufTechToLinFn xs)
                mapM_ (answerS . unwords . fmap (show . unMod)) $
                  fmap (\lf -> fmap (applyLinFn lf) (take p [0 ..])) steps
