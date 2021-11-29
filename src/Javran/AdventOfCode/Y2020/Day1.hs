{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2020.Day1
  ( main
  )
where

import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IntSet as IS
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import System.Directory
import System.Environment
import System.FilePath.Posix

getInput :: IO IS.IntSet
getInput = do
  fp <- prepareData "2020/day/1/input"
  xs <- fmap (read @Int) . words <$> readFile fp
  pure $ IS.fromList xs

solutions :: IS.IntSet -> [] Int
solutions xs = do
  x <- IS.toAscList xs
  let (_, ys) = IS.split x xs
  let y = 2020 - x
  True <- [IS.member y ys]
  pure $ x * y

solutions2 :: IS.IntSet -> [] Int
solutions2 xs = do
  x <- IS.toAscList xs
  let (_, ys) = IS.split x xs
  y <- IS.toAscList ys
  let (_, zs) = IS.split y ys
      z = 2020 - x - y
  True <- [IS.member z zs]
  pure $ x * y * z

{-
  Ensure that the resource is available locally.

  PROJECT_HOME must be set.

 -}
prepareData :: FilePath -> IO FilePath
prepareData rsc = do
  projectHome <- getEnv "PROJECT_HOME"
  mySession <- getEnv "ADVENT_OF_CODE_SESSION"
  let actualFp = projectHome </> "data" </> "download" </> rsc
      (actualDir, _) = splitFileName actualFp
  createDirectoryIfMissing True actualDir
  e <- doesFileExist actualFp
  actualFp
    <$ unless
      e
      (do
         mgr <- newManager tlsManagerSettings
         req <- parseRequest $ "https://adventofcode.com" </> rsc
         print req
         resp <- httpLbs req mgr
         guard $ responseStatus resp == status200
         BSL.writeFile actualFp $ responseBody resp)

main :: IO ()
main = do
  xs <- getInput
  print (head (solutions xs))
  print (head (solutions2 xs))
