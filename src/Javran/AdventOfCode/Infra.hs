{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-
  This module contains intrastructure that keep things working.
 -}

module Javran.AdventOfCode.Infra
  ( prepareDataPath
  , SubCmdHandlers
  , dispatchToSubCmds
  , Solution (..)
  , SolutionContext (..)
  , runSolutionWithInputGetter
  , runSolutionWithLoginInput
  , runSolutionWithExampleInput
  , SomeSolution (..)
  , exampleRawInputRelativePath
  , consumeAllWithReadP
  , decimal1P
  , extractSection
  , ExtractSectionCallback
  , mayEditFileWithSpecialSection
  , consumeExtraLeadingLines
  )
where

import Control.Monad
import Control.Once
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import Data.IORef
import Data.Maybe
import Data.Proxy
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import GHC.Generics
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.IO
import qualified System.IO.Strict
import Text.ParserCombinators.ReadP
import qualified Turtle.Bytes as TBytes

consumeAllWithReadP :: ReadP a -> String -> Maybe a
consumeAllWithReadP p xs = case readP_to_S (p <* eof) xs of
  [(v, "")] -> pure v
  _ -> Nothing

decimal1P :: (Read i, Integral i) => ReadP i
decimal1P = read <$> munch1 isDigit

{-
  Ensure that the resource is available locally.
 -}
prepareDataPath :: FilePath -> IO FilePath
prepareDataPath rsc = do
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
             ["--cookie", "session=" <> T.pack mySession, T.pack url, "--fail"]
             ""
         BS.writeFile actualFp raw)

type SubCmdHandlers = [(String, String -> IO ())]

dispatchToSubCmds :: String -> SubCmdHandlers -> IO ()
dispatchToSubCmds cmdHelpPrefix subCmdHandlers =
  getArgs >>= \case
    subCmd : args
      | Just handler <- lookup subCmd subCmdHandlers ->
        withArgs args (handler (cmdHelpPrefix <> subCmd <> " "))
    _ -> do
      forM_ subCmdHandlers $ \(sub, _) ->
        putStrLn $ cmdHelpPrefix <> sub <> " ..."
      exitFailure

getRawInput :: Int -> Int -> IO BSL.ByteString
getRawInput yyyy dd = prepareDataPath rsc >>= BSL.readFile
  where
    rsc = show yyyy </> "day" </> show dd </> "input"

exampleRawInputRelativePath :: Int -> Int -> FilePath
exampleRawInputRelativePath yyyy dd =
  "data"
    </> "testdata"
    </> show yyyy
    </> "day"
    </> show dd

getExampleRawInput :: Int -> Int -> IO BSL.ByteString
getExampleRawInput yyyy dd = do
  projectHome <- getEnv "PROJECT_HOME"
  {-
    Avoid using Paths_ here, as this module is the dependency of many solutions,
    we don't want to pull in all of them because of some random updates to Paths_ file.
   -}
  let subPath = exampleRawInputRelativePath yyyy dd
      exampleInputFileName = "example.input.txt"
      fp =
        projectHome
          </> subPath
          </> exampleInputFileName
  e <- doesFileExist fp
  if e
    then BSL.readFile fp
    else do
      createDirectoryIfMissing True (projectHome </> subPath)
      hPutStrLn stderr $
        "Sample file for Solution " <> show (yyyy, dd) <> " does not exist."
      hPutStrLn stderr $
        "Please write its content to: " <> (projectHome </> subPath </> exampleInputFileName)
      exitFailure

data SolutionContext = SolutionContext
  { getInputS :: IO String
  , getInputT :: IO T.Text
  , answerS :: String -> IO ()
  , answerShow :: forall a. Show a => a -> IO ()
  , answerT :: T.Text -> IO ()
  }

moduleNameToYearDayP :: ReadP (Int, Int)
moduleNameToYearDayP =
  (,)
    <$> (do
           _ <- string "Javran.AdventOfCode.Y"
           decimal1P)
    <*> (do
           _ <- string ".Day"
           decimal1P)

class Solution sol where
  -- year and day
  solutionIndex :: forall p. p sol -> (Int, Int)
  default solutionIndex
    :: forall d f.
    ( Generic sol
    , Rep sol ~ M1 D d f
    , Datatype d
    )
    => forall p. p sol -> (Int, Int)
  solutionIndex _ =
    fromJust $
      consumeAllWithReadP
        moduleNameToYearDayP
        (moduleName (from @sol undefined))

  solutionSolved :: forall p. p sol -> Bool
  solutionSolved _ = True

  solutionRun :: forall p. p sol -> SolutionContext -> IO ()

runSolutionWithInputGetter :: forall p sol. Solution sol => p sol -> (Int -> Int -> IO BSL.ByteString) -> IO T.Text
runSolutionWithInputGetter p inputGetter = do
  let (yyyy, dd) = solutionIndex p
  getInputBs <- once (inputGetter yyyy dd)
  outRef <- newIORef @TLB.Builder ""
  let getInputT = decodeUtf8 . BSL.toStrict <$> getInputBs
      getInputS = T.unpack <$> getInputT
      answerT output =
        atomicModifyIORef' outRef (\b -> (b <> TLB.fromText output <> "\n", ()))
      answerS output =
        atomicModifyIORef' outRef (\b -> (b <> TLB.fromString output <> "\n", ()))
      answerShow :: forall a. Show a => a -> IO ()
      answerShow = answerS . show
  solutionRun
    p
    SolutionContext
      { getInputS
      , getInputT
      , answerT
      , answerS
      , answerShow
      }
  answer <- readIORef outRef
  pure $ TL.toStrict $ TLB.toLazyText answer

runSolutionWithExampleInput :: forall p sol. Solution sol => p sol -> IO T.Text
runSolutionWithExampleInput p = runSolutionWithInputGetter p getExampleRawInput

runSolutionWithLoginInput :: forall p sol. Solution sol => p sol -> IO T.Text
runSolutionWithLoginInput p = runSolutionWithInputGetter p getRawInput

data SomeSolution
  = forall sol. Solution sol => SomeSolution (Proxy sol)

type ExtractSectionCallback line result =
  [line] -> line -> [line] -> line -> [line] -> result

extractSection
  :: Eq t
  => t -- begin marker
  -> t -- end marker
  -> a -- default value if this section does not exist
  -> ExtractSectionCallback t a
  -> [t] -- file lines
  -> a
extractSection beginMarker endMarker defVal onSuccess xs = fromMaybe defVal $ do
  (ys0, bm : remaining0) <- pure $ span (/= beginMarker) xs
  (ys1, em : remaining1) <- pure $ span (/= endMarker) remaining0
  pure $ onSuccess ys0 bm ys1 em remaining1

{-
  Examples could contain smaller examples with smaller extra parameters than
  the actual input - to allow solutions to deal with this situation,
  a special section can be introduced to the input data,
  which must be the first section of the input file:

  > # EXAMPLE_EXTRA_BEGIN
  > ... some extra lines ...
  > ... some more extra lines ...
  > # EXAMPLE_EXTRA_END

  and `consumeExtraLeadingLines` cuts this extra section
  as a separate bit of input for a solution to consume.
 -}
consumeExtraLeadingLines :: String -> (Maybe [String], String)
consumeExtraLeadingLines raw =
  extractSection
    "# EXAMPLE_EXTRA_BEGIN"
    "# EXAMPLE_EXTRA_END"
    (Nothing, raw)
    (\_pre _bm sec _em post -> (Just sec, unlines post))
    (lines raw)

mayEditFileWithSpecialSection
  :: FilePath
  -> String
  -> String
  -> String
  -> ExtractSectionCallback
       String
       ( [String]
       , Maybe Bool {- if this part is `Just False`, we guarantee not to scrutinize `fst` part -}
       )
  -> IO ()
mayEditFileWithSpecialSection fp prefix bm em extractSecCb = do
  mainModuleContents <- System.IO.Strict.readFile fp
  let contentLines = lines mainModuleContents
      editResult :: ([String], Maybe Bool)
      editResult =
        extractSection
          bm
          em
          -- when the section is not found.
          (contentLines, Nothing)
          extractSecCb
          contentLines
  case editResult of
    (_, Nothing) -> do
      putStrLn $ prefix <> "Abort editing as no section is recognized."
    (_, Just False) -> do
      putStrLn $ prefix <> "No edit required."
    (xs, Just True) -> do
      writeFile fp (unlines xs)
      putStrLn $ prefix <> "File edited."
