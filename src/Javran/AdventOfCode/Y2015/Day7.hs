{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2015.Day7
  (
  )
where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Char
import Data.List
import qualified Data.Map.Strict as M
import Data.Word
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.TestExtra
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day7 deriving (Generic)

data Gate = Gate
  { gIn :: [Either String Word16]
  , gAction :: [Word16] -> Word16
  , gOut :: String
  }

gateP :: ReadP Gate
gateP = do
  let operandP =
        (Left <$> munch1 isAsciiLower)
          <++ (Right . fromIntegral @_ @Word16 <$> decimal1P @Int)
      arrRhsP = strP "-> " *> munch1 isAsciiLower
      notP =
        strP "NOT " *> do
          a <- operandP <* char ' '
          r <- arrRhsP
          pure $ Gate {gIn = [a], gAction = \[x] -> complement x, gOut = r}
  notP <++ do
    a <- operandP <* char ' '
    ahead <- look
    if "-> " `isPrefixOf` ahead
      then do
        r <- arrRhsP
        pure $ Gate {gIn = [a], gAction = \[x] -> x, gOut = r}
      else do
        f <-
          ((.&.) <$ string "AND ")
            <++ ((.|.) <$ string "OR ")
            <++ ((\u v -> unsafeShiftL u (fromIntegral v)) <$ string "LSHIFT ")
            <++ ((\u v -> unsafeShiftR u (fromIntegral v)) <$ string "RSHIFT ")
        b <- operandP <* char ' '
        r <- arrRhsP
        pure $ Gate {gIn = [a, b], gAction = \[x, y] -> f x y, gOut = r}

type System = ([Gate], M.Map String Word16)

{-
  Steps the system, gates are discharged whenever all of their inputs are available,
  and then resulting outputs are written back to memory and input of remaining gates.
 -}
step :: System -> System
step (gates, m) = (gates'', m')
  where
    m' = M.union newM m
    newM = M.fromList pairs
    (gates', pairs) = partitionEithers $ fmap eval gates
    gates'' = fmap (\g@Gate {gIn} -> g {gIn = fmap update gIn}) gates'
      where
        update x = case x of
          Right _ -> x
          Left k -> maybe x Right (newM M.!? k)
    eval g@Gate {gIn, gAction, gOut} =
      if null ls
        then
          let result = gAction rs
           in Right (gOut, result)
        else Left g
      where
        (ls, rs) = partitionEithers gIn

instance Solution Day7 where
  solutionRun _ SolutionContext {getInputS, answerShow, answerS} = do
    (extraOp, rawInput) <- consumeExtra getInputS
    let gates = consumeOrDie (many (gateP <* char '\n')) rawInput
    case extraOp of
      Just _ -> do
        let progression = iterate step (gates, M.empty)
            (_, m) : _ = dropWhile (not . null . fst) progression
        forM_ (M.toAscList m) \(k, v) ->
          answerS $ k <> ": " <> show v
      Nothing -> do
        ans1 <- do
          let progression = iterate step (gates, M.empty)
              (_, m) : _ = dropWhile (M.notMember "a" . snd) progression
              ans1 = m M.! "a"
          ans1 <$ answerShow ans1
        do
          let gates2 =
                Gate {gIn = [Right ans1], gAction = \[x] -> x, gOut = "b"} :
                filter ((/= "b") . gOut) gates
              progression = iterate step (gates2, M.empty)
              (_, m) : _ = dropWhile (M.notMember "a" . snd) progression
              ans2 = m M.! "a"
          answerShow ans2
