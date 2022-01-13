{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2017.Day8
  (
  )
where

import Control.Monad.State.Strict
import Control.Monad.Writer.CPS
import Data.Char
import qualified Data.Map.Strict as M
import Data.Semigroup
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day8 deriving (Generic)

type Reg = String

type Instr = ((Reg, Int), (Reg, Int -> Bool))

instrP :: ReadP Instr
instrP = do
  let regP = munch1 isAlpha
      intP = readS_to_P (reads @Int)
  r0 <- regP <* char ' '
  incr <-
    (string "inc " *> intP)
      <++ (string "dec " *> (negate <$> intP))
  _ <- string " if "
  r1 <- regP
  _ <- char ' '
  op <-
    let lit ~> f = f <$ string lit
     in foldr1
          (<++)
          [ ">=" ~> (>=)
          , "==" ~> (==)
          , "!=" ~> (/=)
          , "<=" ~> (<=)
          , ">" ~> (>)
          , "<" ~> (<)
          ]
  _ <- char ' '
  n <- intP
  pure ((r0, incr), (r1, \val -> val `op` n))

type Memory = M.Map String Int

type KeepMax = Writer (Maybe (Max Int))

{-
  Here we can:

  - use alterF with a writer,
    drawback being now we are in a writer monad.
  - or just write the value and retrive it again afterwards,
    drawback being having to do lookup twice.

  taking first approach, but I don't think either is clearly better.
 -}
interpret :: Instr -> Memory -> KeepMax Memory
interpret ((mutReg, mutIncr), (condReg, p)) mem =
  if p condVal
    then
      M.alterF
        (\case
           Nothing -> upd mutIncr
           Just v -> upd $ v + mutIncr)
        mutReg
        mem
    else pure mem
  where
    upd newVal = Just newVal <$ tell (Just (Max newVal))
    condVal = fromMaybe 0 $ mem M.!? condReg

interpret2 :: Instr -> StateT Memory KeepMax ()
interpret2 instr = StateT (fmap ((),) . interpret instr)

instance Solution Day8 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (consumeOrDie instrP) . lines <$> getInputS
    let (ans1, Just (Max ans2)) =
          runWriter $
            evalStateT (mapM_ interpret2 xs >> gets maximum) M.empty
    answerShow ans1
    answerShow ans2
