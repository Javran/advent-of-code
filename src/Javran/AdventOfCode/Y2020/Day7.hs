{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2020.Day7
  (
  )
where

import Control.Monad
import qualified Control.Monad.State as State
import Data.Function.Memoize (memoFix)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP

data Day7

type Bag = String

type ContainRule = (Bag, [(Bag, Int)])

containRuleP :: ReadP ContainRule
containRuleP = do
  stuff <- get `manyTill` string " bags contain "
  let items1P :: ReadP [(Bag, Int)]
      items1P = do
        let itemP :: ReadP (Bag, Int)
            itemP = do
              n <- decimal1P @Int
              _ <- char ' '
              let endStr = if n == 1 then " bag" else " bags"
              w <- get `manyTill` string endStr
              pure (w, n)
        itemP `sepBy1` string ", "
  items <- ([] <$ string "no other bags") <++ items1P
  _ <- char '.'
  pure (stuff, items)

instance Solution Day7 where
  solutionIndex _ = (2020, 7)
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    rawRules <- lines <$> getInputS
    let containRules = fmap (fromJust . consumeAllWithReadP containRuleP) rawRules
        rulesMap = M.fromList containRules
        containRelMap = M.fromListWith (<>) $ do
          (container, containees) <- containRules
          (ee, _) <- containees
          pure (ee, [container])
        dfs cur = do
          e <- State.gets (S.member cur)
          unless e $ do
            -- visit this node.
            State.modify (S.insert cur)
            case containRelMap M.!? cur of
              Nothing -> pure ()
              Just xs -> mapM_ dfs xs
    answerShow $ S.size (State.execState (dfs "shiny gold") S.empty) -1
    let countBags = memoFix $ \query b ->
          1
            + case rulesMap M.!? b of
              Nothing -> 0
              Just xs -> sum $ fmap (\(b', c) -> c * query b') xs
    answerShow (countBags "shiny gold" - 1)
