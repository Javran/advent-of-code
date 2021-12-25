{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2019.Day6
  (
  )
where

import Data.Function.Memoize (memoFix)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, many)

data Day6 deriving (Generic)

orbitRelationP :: ReadP (String, String)
orbitRelationP =
  (,) <$> (munch1 (/= ')') <* char ')')
    <*> munch1 (const True)

instance Solution Day6 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (consumeOrDie orbitRelationP) . lines <$> getInputS
    let diag = M.fromListWith (<>) do
          (k, v) <- xs
          pure (k, S.singleton v)
        countOrbits nodes depth curSum =
          if S.null nodes
            then curSum
            else
              let nodes' = S.unions do
                    n <- S.toList nodes
                    pure $ fromMaybe S.empty (diag M.!? n)
               in countOrbits nodes' (depth + 1) (curSum + depth * S.size nodes)
    answerShow (countOrbits (S.singleton "COM") 0 0)
    let parents = M.fromList do
          (k, v) <- xs
          pure (v, k)
        descendantsOf :: String -> S.Set String
        descendantsOf = memoFix $ \query node -> case diag M.!? node of
          Nothing -> S.empty
          Just s -> S.unions (s : fmap query (S.toList s))
        countParents target node acc =
          if S.member target (descendantsOf p)
            then acc
            else countParents target p (acc + 1 :: Int)
          where
            p = parents M.! node
    -- TODO: https://en.wikipedia.org/wiki/Tarjan's_off-line_lowest_common_ancestors_algorithm
    answerShow $ countParents "SAN" "YOU" (countParents "YOU" "SAN" 0)
