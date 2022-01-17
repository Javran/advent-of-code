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

module Javran.AdventOfCode.Y2016.Day5
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import qualified Crypto.Hash.MD5 as Md5
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy.Builder.ASCII as Builder
import Data.Char
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Numeric
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day5 deriving (Generic)

extractDigit :: Builder.Builder -> Maybe Char
extractDigit d = do
  let h = Md5.hashlazy (Builder.toLazyByteString d)
  let a = BS.index h 2
  guard $ BS.index h 0 == 0 && BS.index h 1 == 0 && (0xF0 .&. a) == 0
  pure $ head $ showHex (0x0F .&. a) ""

extractDigit2 :: Builder.Builder -> Maybe (Int, Char)
extractDigit2 d = do
  let h = Md5.hashlazy (Builder.toLazyByteString d)
  let a = BS.index h 2
  guard $ BS.index h 0 == 0 && BS.index h 1 == 0 && (0xF0 .&. a) == 0
  let pos = 0x0F .&. a
  guard $ pos >= 0 && pos <= 7
  pure $ (fromIntegral pos, head $ showHex (unsafeShiftR (BS.index h 3) 4) "")

searchSpace :: String -> [Builder.Builder]
searchSpace p = fmap gen [0 :: Int ..]
  where
    prefix = Builder.string7 p
    gen i = prefix <> Builder.string7 (show i)

buildCode :: [Int] -> IM.IntMap a -> [(Int, a)] -> [a]
buildCode missing assigned ~((pos, ch) : xs)
  | null missing = IM.elems assigned
  | pos `elem` missing = buildCode (delete pos missing) (IM.insert pos ch assigned) xs
  | otherwise = buildCode missing assigned xs

instance Solution Day5 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerS} = do
    inp <- head . lines <$> getInputS
    let runPart1 = False
    let ss = searchSpace inp
    when runPart1 do
      answerS (take 8 $ mapMaybe extractDigit ss)
    do
      let code = buildCode [0..7] IM.empty (mapMaybe extractDigit2 ss)
      answerS code