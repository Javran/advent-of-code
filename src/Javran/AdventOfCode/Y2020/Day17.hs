{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Javran.AdventOfCode.Y2020.Day17
  ( pprUniv
  )
where


import Control.Applicative
import Control.Monad
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Semigroup
import qualified Data.Set as S
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (many)

data Day17

cellP :: ReadP Bool
cellP = (False <$ char '.') <++ (True <$ char '#')

type Coord = (Int, Int, Int)

type Coord2 = (Int, Int, Int, Int)

type Universe = S.Set Coord

neighborhoods :: Coord -> [] Coord
neighborhoods c@(x, y, z) = do
  x' <- [x -1 .. x + 1]
  y' <- [y -1 .. y + 1]
  z' <- [z -1 .. z + 1]
  let c' = (x', y', z')
  guard $ c /= c'
  pure c'

neighborhoods2 :: Coord2 -> [] Coord2
neighborhoods2 c@(x, y, z, w) = do
  x' <- [x -1 .. x + 1]
  y' <- [y -1 .. y + 1]
  z' <- [z -1 .. z + 1]
  w' <- [w -1 .. w + 1]
  let c' = (x', y', z', w')
  guard $ c /= c'
  pure c'

countNeighborhoods :: Ord t => (t -> [t]) -> S.Set t -> M.Map t Int
countNeighborhoods getNeighborhoods u = M.fromListWith (+) $ do
  c <- S.toList u
  c' <- getNeighborhoods c
  pure (c', 1)

stepUniv :: Ord t => (t -> [t]) -> S.Set t -> S.Set t
stepUniv getNeighborhoods u = S.filter keepAlive u `S.union` newBorns
  where
    newBorns = S.filter comeToLive $ S.difference (M.keysSet nCounts) u
    comeToLive c = case nCounts M.!? c of
      Nothing -> False
      Just cnt -> cnt == 3
    keepAlive c = case nCounts M.!? c of
      Nothing -> False
      Just cnt -> cnt `elem` [2, 3]
    nCounts = countNeighborhoods getNeighborhoods u

pprUniv :: Universe -> IO ()
pprUniv u = do
  let Just
        ( (Min minX, Max maxX)
          , (Min minY, Max maxY)
          , (Min minZ, Max maxZ)
          ) =
          foldMap
            (\(x, y, z) ->
               Just
                 ( (Min x, Max x)
                 , (Min y, Max y)
                 , (Min z, Max z)
                 ))
            u
  putStrLn $
    intercalate
      ", "
      [ "x range: " <> show (minX, maxX)
      , "y range: " <> show (minY, maxY)
      , "z range: " <> show (minZ, maxZ)
      ]

  forM_ [minZ .. maxZ] $ \z -> do
    putStrLn $ "z=" <> show z
    forM_ [minY .. maxY] $ \y -> do
      let tr x = if S.member (x, y, z) u then '#' else '.'
      putStrLn $ fmap tr [minX .. maxX]
    putStrLn ""

instance Solution Day17 where
  solutionIndex _ = (2020, 17)
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <-
      fmap (fromJust . consumeAllWithReadP (many cellP)) . lines <$> getInputS
    let initUniv :: Universe
        initUniv = S.fromList $ do
          (r, row) <- zip [0 ..] xs
          (c, cell) <- zip [0 ..] row
          guard cell
          -- let x-axis be horizontal and y-axis vertical.
          pure (c, r, 0)
        initUniv2 = S.map (\(x,y,z) -> (x,y,z,0)) initUniv

    answerShow $ S.size (iterate (stepUniv neighborhoods) initUniv !! 6)
    answerShow $ S.size (iterate (stepUniv neighborhoods2) initUniv2 !! 6)
