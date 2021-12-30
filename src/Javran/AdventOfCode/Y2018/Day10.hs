{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2018.Day10
  (
  )
where


import Control.Monad
import Data.List.Split hiding (sepBy)
import qualified Data.Set as S
import Javran.AdventOfCode.Prelude
import Linear.Affine
import Linear.V2
import Linear.Vector
import Text.ParserCombinators.ReadP hiding (count, get, many)

data Day10 deriving (Generic)

type Pt = (Point V2 Int, V2 Int)

ptP :: ReadP Pt
ptP = do
  let intP = readS_to_P (reads @Int)
      pairP = skipSpaces *> ((,) <$> intP <*> (string ", " *> skipSpaces *> intP))
  _ <- string "position=<"
  (x, y) <- pairP
  _ <- string "> velocity=<"
  (vx, vy) <- pairP
  _ <- char '>'
  pure (P (V2 x y), V2 vx vy)

stepPt :: Int -> Pt -> Pt
stepPt n (pos, vel) = (pos .+^ (vel ^* n), vel)

getRange :: [Pt] -> MinMax2D Int Int
getRange = fromJust . foldMap (\(P (V2 x y), _vel) -> Just $ minMax2D (x, y))

getArea :: [Pt] -> Int
getArea pts = (maxX - minX + 1) * (maxY - minY + 1)
  where
    MinMax2D ((minX, maxX), (minY, maxY)) = getRange pts

{-
  This one is solved in some manual manner: it makes sense that
  we just need to find some time around a specific time t when
  the bounding rectangle of all points is the smallest.

  Turns out for this puzzle we just need to find the time with minimal area.

  For actually solving this by algorithm, an initial estimate of t
  is found by taking giant steps until we find the area value increasing.
  Then this range is further cut down by https://en.wikipedia.org/wiki/Ternary_search.
 -}
instance Solution Day10 where
  solutionRun _ SolutionContext {getInputS, answerShow, answerS, terminal} = do
    xs <- fmap (consumeOrDie ptP) . lines <$> getInputS
    let areaOnStepN n = getArea $ fmap (stepPt n) xs
        giantSteps =
          {-
            Take some giant steps (in this case, 1000 at a time) to find a
            potential range, we expect this area to go all the way down
            then up again, so we are looking for a sliding window [a,b,c], where
            b <= c, and we can use (a,c) as our initial range.

            *I think* even if we overshot at the first step we are still fine:
            since we are taking a sliding window and the range [a..c] would make sure
            that we do cover the minimum.
           -}
          fmap (\n -> (n, areaOnStepN n)) [0, 1000 ..]
        [(lowN, _), _, (highN, _)] : _ =
          dropWhile (\[_, (_, u), (_, v)] -> u > v) $ divvy 3 1 giantSteps
        ternary l r =
          case areaOnStepN m1 `compare` areaOnStepN m2 of
            GT -> if m1 == l then mid else ternary m1 r
            EQ -> if (m1, m2) == (l, r) then mid else ternary m1 m2
            LT -> if m2 == r then mid else ternary l m2
          where
            oneThird = (r - l) `quot` 3
            m1 = l + oneThird
            m2 = r - oneThird
            mid = (l + r) `quot` 2

    let targetN = ternary lowN highN
        pts = fmap (stepPt targetN) xs
        ptsSet = S.fromList (fmap fst pts)
        MinMax2D ((minX, maxX), (minY, maxY)) = getRange pts
    forM_ [minY .. maxY] \y -> do
      let (tt, ff) = case terminal of
            Nothing -> ("#", ".")
            Just _ -> ("█", " ")
      let render x = if S.member (P (V2 x y)) ptsSet then tt else ff
      answerS $ concatMap render [minX .. maxX]
    answerShow targetN
