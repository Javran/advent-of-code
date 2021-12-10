{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2019.Day2
  (
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Vector.Unboxed as VU
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2019.IntCode
import qualified ListT

data Day2 deriving (Generic)

instance Solution Day2 where
  solutionRun _ SolutionContext {getInputS, answerShow, answerS} = do
    (extraOps, rawInput) <- consumeExtraLeadingLines <$> getInputS
    let xs = fmap (read @Int) . splitOn "," . head . lines $ rawInput
        mem = VU.fromList xs
    case extraOps of
      Nothing -> do
        -- running with login data.
        let runWithInput a b = do
              let mem' = mem VU.// [(1, a), (2, b)]
              (mem'', []) <- runProgram mem' []
              pure (mem'' VU.! 0)
        p1 <- runWithInput 12 2
        answerShow p1
        let target = 19690720
            performSearch :: ListT.ListT IO (Int, Int)
            performSearch = ListT.take 1 do
              a <- ListT.fromFoldable [0 .. 99]
              b <- ListT.fromFoldable [0 .. 99]
              result <- liftIO $ runWithInput a b
              guard $ result == target
              pure (a, b)
        (n, v) : _ <- ListT.toList performSearch
        answerShow (100 * n + v)
      Just _ -> do
        (mem', []) <- runProgram mem []
        answerS $ intercalate "," . fmap show $ VU.toList mem'
