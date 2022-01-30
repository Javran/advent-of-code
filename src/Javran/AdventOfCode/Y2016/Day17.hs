{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Javran.AdventOfCode.Y2016.Day17
  (
  )
where

import Control.Monad
import qualified Crypto.Hash.MD5 as Md5
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.DList as DL
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Javran.AdventOfCode.GridSystem.RowThenCol.Uldr
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2016.Day14 (splitHighLow)

data Day17 deriving (Generic)

data Node = Node
  { nLoc :: Coord
  , nPath :: DL.DList Dir
  , nCtx :: Md5.Ctx
  }

adjacents :: Node -> [] Node
adjacents Node {nLoc, nPath, nCtx} = do
  let dirs =
        zipWith
          (\d v -> (d, v > 0xA))
          [U, D, L, R]
          [uAvail, dAvail, lAvail, rAvail]
        where
          (uAvail, dAvail) = splitHighLow $ BS.index raw 0
          (lAvail, rAvail) = splitHighLow $ BS.index raw 1
          raw = Md5.finalize nCtx
  (d, avail) <- dirs
  let extra = BSC.pack $ show d
      nLoc' = applyDir d nLoc
  guard $ avail && inRange ((0, 0), (3, 3)) nLoc'
  pure
    Node
      { nLoc = nLoc'
      , nPath = nPath <> DL.singleton d
      , nCtx = Md5.update nCtx extra
      }

bfs :: S.Set BSC.ByteString -> Seq.Seq Node -> [DL.DList Dir]
bfs discovered = \case
  Seq.Empty -> []
  cur@Node {nLoc, nPath} Seq.:<| q1 ->
    if nLoc == (3, 3)
      then nPath : bfs discovered q1
      else
        let nexts = do
              ns'@Node {nCtx = Md5.Ctx h'} <- adjacents cur
              guard $ S.notMember h' discovered
              pure (ns', h')
            discovered' = foldr (\(_, h) -> S.insert h) discovered nexts
            q2 = q1 <> Seq.fromList (fmap fst nexts)
         in bfs discovered' q2

instance Solution Day17 where
  solutionRun _ SolutionContext {getInputS, answerShow, answerS} = do
    prefix <- head . lines <$> getInputS
    let initNode =
          Node
            { nLoc = (0, 0)
            , nPath = DL.empty
            , nCtx = Md5.start (BSC.pack prefix)
            }
        Md5.Ctx d = nCtx initNode
        solutions :: [DL.DList Dir]
        solutions = bfs (S.singleton d) (Seq.singleton initNode)
    answerS $ concatMap show $ head solutions
    answerShow $ length $ last solutions
