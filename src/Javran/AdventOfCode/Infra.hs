{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-
  This module contains intrastructure that keep things working.
 -}

module Javran.AdventOfCode.Infra
  ( SubCmdHandlers
  , SubCmdContext (..)
  , Solution (..)
  , SolutionContext (..)
  , runSolutionWithInputGetter
  , runSolutionWithLoginInput
  , runSolutionWithExampleInput
  , SomeSolution (..)
  , exampleRawInputRelativePath
  , consumeAllWithReadP
  , consumeAllWithReadPDebug
  , consumeAllWithReadPDebugShow
  , consumeOrDie
  , decimal1P
  , extractSection
  , ExtractSectionCallback
  , mayEditFileWithSpecialSection
  , consumeExtraLeadingLines
  , unreachable
  , todo
  )
where

import Control.Monad
import Control.Once
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import Data.IORef
import Data.Maybe
import Data.Proxy
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import GHC.Generics
import GHC.Stack.Types (HasCallStack)
import Javran.AdventOfCode.Network
import Network.HTTP.Client (Manager)
import System.Console.Terminfo
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.IO
import qualified System.IO.Strict
import Text.ParserCombinators.ReadP

unreachable :: HasCallStack => a
unreachable = error "unreachable"

todo :: HasCallStack => a
todo = error "todo"

consumeOrDie :: ReadP a -> String -> a
consumeOrDie p = fromJust . consumeAllWithReadP p

consumeAllWithReadP :: ReadP a -> String -> Maybe a
consumeAllWithReadP p xs = case readP_to_S (p <* eof) xs of
  [(v, "")] -> pure v
  _ -> Nothing

consumeAllWithReadPDebug :: ReadP a -> String -> Either String a
consumeAllWithReadPDebug p xs = case readP_to_S (p <* eof) xs of
  [] -> Left "No alternatives."
  [(v, "")] -> pure v
  ys@(_ : _) ->
    Left $
      "Parsing is ambiguious, " <> show (length ys) <> "alternatives found."

consumeAllWithReadPDebugShow :: Show a => ReadP a -> String -> Either String a
consumeAllWithReadPDebugShow p xs = case readP_to_S (p <* eof) xs of
  [] -> Left "No alternatives."
  [(v, "")] -> pure v
  ys@(_ : _) ->
    Left $
      "Parsing is ambiguious. Alternatives: " <> show ys

decimal1P :: (Read i, Integral i) => ReadP i
decimal1P = read <$> munch1 isDigit

data SubCmdContext = SubCmdContext
  { mTerm :: Maybe Terminal
  , cmdHelpPrefix :: String
  , manager :: Manager
  }

type SubCmdHandlers = [(String, SubCmdContext -> IO ())]

getRawLoginInput :: Manager -> Int -> Int -> IO BSL.ByteString
getRawLoginInput mgr yyyy dd = do
  projectHome <- getEnv "PROJECT_HOME"
  mySession <- getEnv "ADVENT_OF_CODE_SESSION"
  let rsc = show yyyy </> "day" </> show dd </> "input"
      actualFp = projectHome </> "data" </> "download" </> rsc
      (actualDir, _) = splitFileName actualFp
  createDirectoryIfMissing True actualDir
  e <- doesFileExist actualFp
  if e
    then BSL.readFile actualFp
    else do
      raw <- fetchInputData mgr (BSC.pack mySession) yyyy dd
      BSL.writeFile actualFp raw
      pure raw

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
  , terminal :: Maybe Terminal
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

runSolutionWithInputGetter
  :: forall p sol.
  Solution sol
  => p sol
  -> (Int -> Int -> IO BSL.ByteString)
  -> Bool
  -> Maybe Terminal
  -> IO T.Text
runSolutionWithInputGetter p inputGetter interleaveAnswer mTerm = do
  let (yyyy, dd) = solutionIndex p
  getInputBs <- once (inputGetter yyyy dd)
  outRef <- newIORef @TLB.Builder ""
  let getInputT = decodeUtf8 . BSL.toStrict <$> getInputBs
      getInputS = T.unpack <$> getInputT
      mFancyTermOutputLn :: Maybe (String -> IO ())
      mFancyTermOutputLn = do
        let mFg = do
              term <- mTerm
              (term,) <$> getCapability term (withForegroundColor @TermOutput)
        case mFg of
          Nothing -> Nothing
          Just (term, fg) ->
            pure $ \xs -> runTermOutput term $ fg Cyan (termText (xs <> "\n"))
      answerT output = do
        when interleaveAnswer $ do
          case mFancyTermOutputLn of
            Nothing -> T.putStrLn $ "Answer: " <> output
            Just termOutput -> termOutput (T.unpack output)
        atomicModifyIORef' outRef (\b -> (b <> TLB.fromText output <> "\n", ()))
      answerS :: String -> IO ()
      answerS output = do
        when interleaveAnswer $ do
          case mFancyTermOutputLn of
            Nothing -> putStrLn $ "Answer: " <> output
            Just termOutput -> termOutput output
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
      , terminal = mTerm
      }
  answer <- readIORef outRef
  pure $ TL.toStrict $ TLB.toLazyText answer

runSolutionWithExampleInput
  :: forall p sol.
  Solution sol
  => p sol
  -> Bool
  -> Maybe Terminal
  -> IO T.Text
runSolutionWithExampleInput p = runSolutionWithInputGetter p getExampleRawInput

runSolutionWithLoginInput
  :: forall p sol.
  Solution
    sol
  => p sol
  -> Manager
  -> Bool
  -> Maybe Terminal
  -> IO T.Text
runSolutionWithLoginInput p mgr = runSolutionWithInputGetter p (getRawLoginInput mgr)

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
