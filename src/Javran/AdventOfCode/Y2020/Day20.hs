{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2020.Day20
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Bool
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (many)

data Day20

tileLen :: Int
tileLen = 10

halfLen :: Int
halfLen = tileLen `quot` 2

{-
  (<lo>, <hi>)

  for (r,c):

  - when r < 5: `c + r * 10`, index into <lo>
  - when r >= 5: `c + (r-5)*10`, index into <hi>

 -}
newtype PackedTile = PackedTile (Word64, Word64)

toPackedTile :: [[Bool]] -> PackedTile
toPackedTile xs =
  checkShape
    `seq` PackedTile (packToWord loXs, packToWord hiXs)
  where
    checkShape =
      (length xs == tileLen
         || error "invalid # of rows")
        && (let (y : ys) = fmap length xs
             in (y == tileLen && all (== y) ys)
                  || error "invalid # of cols")
    (loXs, hiXs) = splitAt (tileLen `quot` 2) xs
    packToWord :: [[Bool]] -> Word64
    packToWord yys = appEndo setup 0
      where
        setup = mconcat $ do
          (r, ys) <- zip [0 :: Int ..] yys
          (c, y) <- zip [0 :: Int ..] ys
          let i = c + r * tileLen
          if y
            then pure $ Endo (`setBit` i)
            else mempty

type Tile = (Int, Int) -> Bool

toTile :: PackedTile -> Tile
toTile (PackedTile (lo, hi)) (r, c) =
  if r < halfLen
    then testBit lo (c + r * tileLen)
    else testBit hi (c + (r - halfLen) * tileLen)

parseTile :: [String] -> (Int, Tile)
parseTile (t : xs) = (fromJust (consumeAllWithReadP tileNumP t), toTile . toPackedTile . (fmap . fmap) tr $ xs)
  where
    tr '#' = True
    tr '.' = False
    tr _ = error "invalid input"
    tileNumP = string "Tile " *> decimal1P <* char ':'
parseTile _ = undefined

pprTile :: Tile -> IO ()
pprTile t = do
  forM_ [0 .. tileLen -1] $ \r ->
    putStrLn (fmap (\c -> if t (r, c) then '#' else '.') [0 .. tileLen -1])

instance Solution Day20 where
  solutionIndex _ = (2020, 20)
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap parseTile . splitOn [""] . lines <$> getInputS
    mapM_
      (\(i, t) -> do
         putStrLn $ "Tile " <> show i
         pprTile t
         putStrLn "")
      xs
