{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.AdventOfCode.Y2019.Day2
  (
  )
where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import qualified Data.Vector.Unboxed as VU
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2019.IntCode
import qualified ListT

data Day2 deriving (Generic)

instance Solution Day2 where
  solutionRun _ SolutionContext {getInputS, answerShow, answerS} = do
    (ex, rawInput) <- consumeExtraLeadingLines <$> getInputS
    let xs = parseCodeOrDie rawInput
    case ex of
      Nothing -> do
        -- running with login data.
        let runWithInput a b = do
              let xs' = xs & element 1 .~ a & element 2 .~ b
              (mem'', []) <- runProgram xs' []
              pure (mem'' VU.! 0)
        p1 <- runWithInput 12 2
        answerShow p1
        let target = 19690720
        Just (n, v) <- ListT.head do
          a <- ListT.fromFoldable [0 .. 99]
          b <- ListT.fromFoldable [0 .. 99]
          result <- liftIO $ runWithInput a b
          guard $ result == target
          pure (a, b)
        answerShow (100 * n + v)
      Just _ -> do
        (mem', []) <- runProgram xs []
        answerS $ intercalate "," . fmap show $ VU.toList mem'
