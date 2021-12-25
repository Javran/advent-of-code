{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Javran.AdventOfCode.Y2019.Day25
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Control.Monad.Combinators
import Control.Monad.State.Strict
import Data.Char
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Debug.Trace (traceShow)
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2019.Day25.Common
import Javran.AdventOfCode.Y2019.Day25.ResponseParser
import Javran.AdventOfCode.Y2019.IntCode
import Text.ParserCombinators.ReadP hiding (count, get, many, manyTill)

data Day25 deriving (Generic)

allItems :: [String]
allItems =
  [ "cake"
  , "fuel cell"
  , "easter egg"
  , "ornament"
  , "hologram"
  , "dark matter"
  , "klein bottle"
  , "hypercube"
  ]

presetCommands :: [String]
presetCommands =
  [ "north"
  , "north"
  , "east"
  , "east" -- at Crew Quarters
  , "take cake"
  , "west"
  , "west"
  , "south"
  , "south" -- at Hull Breach
  , "south"
  , "west" -- at Hallway
  , "take fuel cell"
  , "west" -- at Warp Drive Maintenance
  , "take easter egg"
  , "east"
  , "east"
  , "north" -- at Hull Breach
  , "east"
  , "take ornament"
  , "east"
  , "take hologram"
  , "east"
  , "take dark matter"
  , "north"
  , "north"
  , "east"
  , "take klein bottle"
  , "north"
  , "take hypercube"
  , "north" -- at Security Checkpoint
  ]
    <> dropEverything
    <> tryAllCombinations
  where
    dropEverything = ["drop " <> x | x <- allItems]

    allCombinations = filterM (const [False, True]) allItems

    tryComb :: [String] -> [String]
    tryComb items =
      ["take " <> item | item <- items] <> ["west"]
        <> ["drop " <> item | item <- items]
    tryAllCombinations = do
      comb <- allCombinations
      tryComb comb

data ExplorerState = ExplorerState
  { esGraph :: M.Map String (M.Map Dir String)
  , esLocation :: String
  , esUnknowns :: M.Map String (S.Set Dir)
  , esInventory :: [String]
  , esProg :: IO AsciiResult
  }

type Explorer = StateT ExplorerState IO

isSafeItem :: String -> Bool
isSafeItem =
  (`notElem`
     [ "escape pod"
     , "giant electromagnet"
     , "infinite loop"
     , "molten lava"
     , "photons"
     ])

updateUnknowns :: String -> [Dir] -> ExplorerState -> ExplorerState
updateUnknowns room dirs es@ExplorerState {esGraph, esUnknowns} =
  es {esUnknowns = esUnknowns'}
  where
    esUnknowns' = foldr upd esUnknowns newUnknowns
      where
        upd d =
          M.alter
            (\case
               Nothing -> Just $ S.singleton d
               Just s -> Just $ S.insert d s)
            room
    newUnknowns =
      filter
        (\d -> case esGraph M.!? room >>= (M.!? d) of
           Nothing -> True
           Just _ -> False)
        dirs

mayTakeItem :: String -> Explorer ()
mayTakeItem item =
  if isSafeItem item
    then do
      r0 <- liftIO =<< gets esProg
      case r0 of
        AsciiNeedCommand k0 -> do
          r1 <- liftIO $ k0 $ "take " <> item
          case r1 of
            AsciiOutput out k1 -> do
              modify (\es -> es {esProg = k1})
              case consumeAllWithReadP responsesP out of
                Just [RespSimple (SimpRespTakeOrDropItem taking item')]
                  | taking
                    , item == item' -> do
                    modify (\es -> es {esInventory = item : esInventory es})
                    pure ()
                r -> error $ "unexpected: " <> show (r, out)
            _ -> error "unexpected"
        _ -> error "unexpected"
    else pure ()

findPathRev
  :: M.Map String (M.Map Dir String)
  -> M.Map String (S.Set Dir)
  -> S.Set String
  -> Seq.Seq (Maybe String, [Dir])
  -> Maybe [Dir]
findPathRev graph unknowns discovered = \case
  Seq.Empty -> Nothing
  (mRoomName, pathRev) Seq.:<| q1 ->
    case mRoomName of
      Nothing -> pure pathRev
      Just roomName -> do
        let nexts = do
              d <- universe @Dir
              nextRoom <-
                (do
                   Just ds <- pure (graph M.!? roomName)
                   Just roomName' <- pure (ds M.!? d)
                   pure (Just roomName'))
                  <|> (do
                         Just ds <- pure (unknowns M.!? roomName)
                         guard $ S.member d ds
                         pure Nothing)
              guard $ maybe True (`S.notMember` discovered) nextRoom
              pure (nextRoom, d : pathRev)
            discovered' :: S.Set String
            discovered' =
              foldr
                (\(i, _) acc -> case i of
                   Nothing -> acc
                   Just room -> S.insert room acc)
                discovered
                nexts
        findPathRev graph unknowns discovered' (q1 <> Seq.fromList nexts)

linkRoom :: String -> Dir -> String -> ExplorerState -> ExplorerState
linkRoom curRoom d newRoom es@ExplorerState {esUnknowns, esGraph} =
  es
    { esGraph =
        M.alter
          (\case
             Nothing -> Just $ M.singleton (oppositeDir d) curRoom
             Just m -> Just $ M.insert (oppositeDir d) curRoom m)
          newRoom
          . M.alter
            (\case
               Nothing -> Just $ M.singleton d newRoom
               Just m -> Just $ M.insert d newRoom m)
            curRoom
          $ esGraph
    , esUnknowns =
        M.adjust (S.delete (oppositeDir d)) newRoom
          . M.adjust (S.delete d) curRoom
          $ esUnknowns
    }

explore :: Explorer ()
explore = do
  r0 <- liftIO =<< gets esProg
  fix
    (\loop result ->
       case result of
         AsciiDone -> error "Unexpected termination."
         AsciiNeedCommand {} -> error "Unexpected NeedCommand."
         AsciiOutput out p -> do
           modify (\es -> es {esProg = p})
           case consumeAllWithReadP responsesP out of
             Just resps -> case resps of
               [RespRoomInfo RoomInfo {riName, riDoorsHereLead, riItemsHere}] -> do
                 -- one room info.
                 modify
                   (updateUnknowns riName riDoorsHereLead
                      . (\es -> es {esLocation = riName}))

                 when (riName == "Security Checkpoint") do
                   {-
                     assuming that Security Checkpoint have only two doors,
                     while one door is explored, the other must be pressure-sensitive stuff,
                     so we can just link the rooms rather than dealing with
                     that special-case response.
                    -}
                   [unkDir] <- gets (S.toList . (M.! riName) . esUnknowns)
                   modify (linkRoom riName unkDir "Pressure-Sensitive Floor")

                 -- take all items that we can.
                 mapM_ mayTakeItem riItemsHere
                 -- path finding
                 mPlan <- gets \ExplorerState {esGraph, esUnknowns} ->
                   findPathRev
                     esGraph
                     esUnknowns
                     (S.singleton riName)
                     (Seq.singleton (Just riName, []))
                 case mPlan of
                   Nothing -> do
                     g <- gets esGraph
                     unk <- gets esUnknowns
                     curLoc <- gets esLocation
                     inv <- gets esInventory
                     liftIO $ do
                       putStrLn $ "Current location: " <> curLoc
                       putStrLn $ "Inventory: " <> intercalate ", " inv
                       putStrLn $ "Graph:"
                       forM_ (M.toList g) \(k, v) -> do
                         putStrLn k
                         forM_ (M.toList v) $ \(d, dst) -> do
                           putStrLn $ "  " <> show d <> " -> " <> dst
                       putStrLn $ "Todos:"
                       forM_ (M.toList unk) \(k, v) -> do
                         unless (null v) do
                           putStrLn $ k <> ": " <> intercalate ", " (fmap show $ S.toList v)
                     error "TODO: proceed to go to Security Checkpoint."
                   Just pathRev -> do
                     let lastStep : knownStepsRev = pathRev
                     -- known steps to execute.
                     forM_ (reverse knownStepsRev) \d -> do
                       AsciiNeedCommand k0 <- liftIO =<< gets esProg
                       AsciiOutput out1 k1 <- liftIO $ k0 (fmap toLower $ show d)
                       let Just [RespRoomInfo RoomInfo {riName = riName'}] =
                             consumeAllWithReadP responsesP out1
                       modify (\es -> es {esProg = k1, esLocation = riName'})

                     AsciiNeedCommand k0 <- liftIO =<< gets esProg
                     curRoom <- gets esLocation
                     r@(AsciiOutput out1 _k1) <- liftIO $ k0 (fmap toLower $ show lastStep)
                     let Just [RespRoomInfo RoomInfo {riName = newRoom}] =
                           consumeAllWithReadP responsesP out1
                     -- update knowledge
                     modify (linkRoom curRoom lastStep newRoom)
                     -- re-enter to let esLocation and esProg update properly.
                     loop r
               [RespRoomInfo {}, RespRoomInfo {}] -> do
                 -- two room info, this only happens with that pressure-sensitive stuff.
                 error "hello"
               [RespSimple {}] -> todo
               _ -> error $ "Unexpected output structure: " <> show resps
             Nothing ->
               error $ "Parse failure, the output was: " <> show out)
    r0

{-
  TODO:

  the sequence of preset command only works on my specific input,
  we need few parts to be more flexible to solve this puzzle in general:

  - room and item detection
  - now we can head to Security Checkpoint
  - bruteforce or do something smarter.

 -}
instance Solution Day25 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    code <- parseCodeOrDie <$> getInputS
    let prog = asciiRun $ startProgramFromFoldable code
        shouldRunProgram = True
    _ <-
      runStateT
        explore
        ExplorerState
          { esGraph = M.empty
          , esLocation = error "unkown"
          , esUnknowns = M.empty
          , esInventory = []
          , esProg = prog
          }
    when shouldRunProgram do
      fix
        (\loop curP curPresetCmds -> do
           result <- curP
           case result of
             AsciiDone -> pure ()
             AsciiNeedCommand k -> do
               (cmd, nextPresetCmds) <- case curPresetCmds of
                 [] -> do
                   xs <- getLine
                   pure (xs, [])
                 (pCmd : cs') -> do
                   putStrLn $ "Enter preset command: " <> pCmd
                   pure (pCmd, cs')
               loop (k cmd) nextPresetCmds
             AsciiOutput outs k -> do
               let r = debugConsumeAllWithReadP responsesP outs
               case r of
                 Right v -> print v
                 Left msg -> do
                   putStrLn $ "warning: parse failure, reason: " <> msg
                   putStr outs
               loop k curPresetCmds)
        prog
        presetCommands
