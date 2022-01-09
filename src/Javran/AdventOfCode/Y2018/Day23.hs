{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2018.Day23
  (
  )
where

import Data.List
import Javran.AdventOfCode.Prelude
import Linear.Affine
import Linear.V3
import Text.ParserCombinators.ReadP hiding (count, get, many)
import Z3.Monad

data Day23 deriving (Generic)

type Loc = Point V3 Int

type Nanobot = (Loc, Int)

nanobotP :: ReadP Nanobot
nanobotP = do
  let intP = readS_to_P (reads @Int)
  loc <- between (string "pos=<") (string ">, r=") do
    [a, b, c] <- intP `sepBy1` char ','
    pure (P (V3 a b c))
  r <- decimal1P
  pure (loc, r)

solve :: [Nanobot] -> Z3 (Loc, (Int, Int))
solve xs = do
  lit0 <- mkInteger 0
  lit1 <- mkInteger 1
  ~[x, y, z] <- mapM mkFreshIntVar ["x", "y", "z"]
  let absZ v = do
        cond <- mkGe v lit0
        v' <- mkUnaryMinus v
        mkIte cond v v'
      distZ u v w = do
        dx <- mkSub [x, u] >>= absZ
        dy <- mkSub [y, v] >>= absZ
        dz <- mkSub [z, w] >>= absZ
        mkAdd [dx, dy, dz]
      countNanobot (P (V3 nx ny nz), r) = do
        ~[nx', ny', nz', r'] <-
          mapM (mkInteger . fromIntegral) [nx, ny, nz, r]
        distToBot <- distZ nx' ny' nz'
        cond <- mkLe distToBot r'
        mkIte cond lit1 lit0
  total <- mapM countNanobot xs >>= mkAdd
  distToOrigin <- distZ lit0 lit0 lit0
  _ <- optimizeMaximize total
  _ <- optimizeMinimize distToOrigin
  Sat <- optimizeCheck []
  model <- optimizeGetModel
  do
    ~[vx, vy, vz, vTotal, vDist] <-
      mapM
        (fmap (fromInteger . fromJust) . evalInt model)
        [x, y, z, total, distToOrigin]
    pure (P (V3 vx vy vz), (vTotal, vDist))

instance Solution Day23 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- fmap (consumeOrDie nanobotP) . lines <$> getInputS
    let (maxLoc, rMax) = maximumBy (comparing snd) xs
    answerShow $ countLength (\(loc, _) -> manhattan maxLoc loc <= rMax) xs
    (_, (_, ans)) <- evalZ3 (solve xs)
    answerShow ans
