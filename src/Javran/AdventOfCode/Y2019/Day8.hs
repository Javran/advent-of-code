{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Javran.AdventOfCode.Y2019.Day8
  (
  )
where


import Data.Coerce
import Data.List
import Data.List.Split hiding (sepBy)
import Data.Monoid
import Data.Ord
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude

data Day8 deriving (Generic)

countLayerPixel :: String -> (Int, (Int, Int))
countLayerPixel =
  coerce
    . foldMap
      (\x ->
         ( x =?= '0'
         , ( x =?= '1'
           , x =?= '2'
           )
         ))
  where
    x =?= y = if x == y then 1 :: Sum Int else 0

{-
  combinePixel u v combines u on top of v.
 -}
combinePixel :: Char -> Char -> Char
combinePixel u v = if u /= '2' then u else v

instance Solution Day8 where
  solutionRun _ SolutionContext {getInputS, answerShow, answerS} = do
    (extraOps, rawInput) <- consumeExtraLeadingLines <$> getInputS
    let (width, height) = case extraOps of
          Nothing -> (25, 6)
          Just [raw] -> read raw
          Just _ -> errInvalid
    let xs = chunksOf (width * height) . head . lines $ rawInput
    let counted = fmap countLayerPixel xs
    let (_, (u, v)) = minimumBy (comparing fst) counted
    answerShow $ u * v
    let pprLine row = answerS $ '|' : fmap tr row <> "|"
        tr '1' = '#'
        tr '0' = ' '
        tr c = c
    mapM_ pprLine $
      chunksOf width $
        foldr1 (zipWith combinePixel) xs
