{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2018.Day14
  (
  )
where

import Control.Monad
import Control.Monad.Writer.Lazy
import Data.Char
import qualified Data.DList as DL
import Data.Foldable
import Data.List.Split hiding (sepBy)
import qualified Data.Sequence as Seq
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.TestExtra

data Day14 deriving (Generic)

type RecipeState = ((Int, Int), Seq.Seq Int)

step :: RecipeState -> (RecipeState, DL.DList Int)
step ((elf0, elf1), xs) =
  (((norm $ elf0 + v0 + 1, norm $ elf1 + v1 + 1), xs'), DL.fromList extra)
  where
    norm = (`rem` newL)
    v0 = Seq.index xs elf0
    v1 = Seq.index xs elf1
    extra =
      let y = v0 + v1
      -- TODO: can't use intToDigits, as it'll generate [] instead of expected [0]
       in if y > 9 then [1, y -10] else [y]
    xs' = xs <> Seq.fromList extra
    newL = Seq.length xs'

recipes :: [Int]
recipes = 3 : 7 : DL.toList (execWriter (gen initSt))
  where
    gen s = writer (step s) >>= gen
    initSt = ((0, 1), Seq.fromList [3, 7])

instance Solution Day14 where
  solutionRun _ SolutionContext {getInputS, answerS, answerShow} = do
    (extraOps, rawInput) <- consumeExtra getInputS
    let rawN = head . lines $ rawInput
        (runPart1, runPart2) = shouldRun extraOps
    when runPart1 do
      let n = read @Int rawN
      answerS (fmap (\v -> chr (v + ord '0')) $ take 10 $ toList $ drop n recipes)
    when runPart2 do
      let nSeq = fmap (read @Int . (: [])) rawN
          (ans, _) : _ =
            dropWhile ((nSeq /=) . snd) $
              zip [0 :: Int ..] (divvy (length nSeq) 1 recipes)
      answerShow ans
