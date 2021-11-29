{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2020.Day1
  ( main
  )
where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.IntSet as IS
import qualified Data.Text as T
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import qualified Turtle.Bytes as TBytes

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
         -- there are too much bullshit involved to get the fucking CookieJar attached to a request for http-client that I won't bother.
         let url = "https://adventofcode.com" </> rsc
         (ExitSuccess, raw) <-
           TBytes.procStrict
             "curl"
             ["--cookie", "session=" <> T.pack mySession, T.pack url]
             ""
         BS.writeFile actualFp raw)

main :: IO ()
main = do
  xs <- getInput
  print (head (solutions xs))
  print (head (solutions2 xs))
