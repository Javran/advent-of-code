{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2019.Day2
  (
  )
where

import Control.Monad
import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IM
import Data.List.Split hiding (sepBy)
import Data.Maybe
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Data.List

data Day2 deriving (Generic)

type Machine = IM.IntMap Int

interpret :: Int -> State Machine ()
interpret pc = do
  let load i = do
        v <- gets (IM.!? i)
        pure $ fromMaybe 0 v
  opCode <- load pc
  let performBin op = do
        lAddr <- load (pc + 1)
        l <- load lAddr
        rAddr <- load (pc + 2)
        r <- load rAddr
        dst <- load (pc + 3)
        modify $ IM.insert dst (op l r)
        interpret (pc + 4)
  case opCode of
    99 -> pure ()
    1 -> performBin (+)
    2 -> performBin (*)
    _ -> error "Something went wrong"

instance Solution Day2 where
  solutionRun _ SolutionContext {getInputS, answerShow, answerS} = do
    (extraOps, rawInput) <- consumeExtraLeadingLines <$> getInputS
    let xs = fmap (read @Int) . splitOn "," . head . lines $ rawInput
        l = length xs
        mem = IM.fromList $ zip [0 ..] xs
    case extraOps of
      Nothing -> do
        -- running with login data.
        let runWithInput a b =
              execState (interpret 0) $
                IM.insert 2 b $
                  IM.insert 1 a mem
            mem' =
              {-
                Note that the problem doesn't seem to specify what are considered
                valid positions - the input override `12` below puts the example program out of bound,
                we can probably do something about it, rather than assuming a 0 input value.
               -}
              runWithInput 12 2
        answerShow $ mem' IM.! 0
        let target = 19690720
            (n, v) : _ = do
              a <- [0 .. 99]
              b <- [0 .. 99]
              guard $ runWithInput a b IM.! 0 == target
              pure (a, b)
        answerShow (100 * n + v)
      Just _ -> do
        let mem' = execState (interpret 0) mem
        answerS $ intercalate "," $ fmap (show . (mem' IM.!)) [0..l-1]
