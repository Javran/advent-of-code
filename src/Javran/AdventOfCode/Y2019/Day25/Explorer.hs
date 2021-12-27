{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2019.Day25.Explorer
  ( ExplorerState (..)
  , explore
  , ExplorerResult
  , runExplorer
  )
where

{-
  The Explorer operates the droid to:

  - explore all the rooms
  - pick up all items that we need
  - and finally head to Security Checkpoint

  TODO: this module still need some cleanup.

 -}

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Data.Char
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2019.Day25.Common
import Javran.AdventOfCode.Y2019.Day25.ResponseParser

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
      AsciiNeedCommand k0 <- liftIO =<< gets esProg
      AsciiOutput out k1 <- liftIO $ k0 $ "take " <> item
      modify (\es -> es {esProg = k1})
      case consumeAllWithReadP responsesP out of
        Just [RespSimple (SimpRespTakeOrDropItem taking item')]
          | taking
            , item == item' -> do
            modify (\es -> es {esInventory = item : esInventory es})
            pure ()
        r -> error $ "unexpected: " <> show (r, out)
    else pure ()

findPathRev
  :: (String -> Bool)
  -> M.Map String (M.Map Dir String)
  -> M.Map String (S.Set Dir)
  -> S.Set String
  -> Seq.Seq (Maybe String, [Dir])
  -> Maybe [Dir]
findPathRev isTargetRoom graph unknowns discovered = \case
  Seq.Empty -> Nothing
  (mRoomName, pathRev) Seq.:<| q1 ->
    case mRoomName of
      Nothing -> pure pathRev
      Just roomName | isTargetRoom roomName -> pure pathRev
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
        findPathRev isTargetRoom graph unknowns discovered' (q1 <> Seq.fromList nexts)

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

type ExplorerResult =
  ( [String] -- full inventory, which is also current inventory
  , Dir -- the direction leading to Pressure-Sensitive Floor
  , IO AsciiResult -- suspended program
  )

moveToSecurityCheckpoint :: Explorer ExplorerResult
moveToSecurityCheckpoint = do
  let secCp = "Security Checkpoint"
  Just planRev <- gets \ExplorerState {esGraph, esUnknowns, esLocation} ->
    findPathRev
      (== secCp)
      esGraph
      esUnknowns
      (S.singleton esLocation)
      (Seq.singleton (Just esLocation, []))
  forM_ (reverse planRev) \d -> do
    AsciiNeedCommand k0 <- liftIO =<< gets esProg
    AsciiOutput out1 k1 <- liftIO $ k0 (fmap toLower $ show d)
    let Just [RespRoomInfo RoomInfo {riName = riName'}] =
          consumeAllWithReadP responsesP out1
    modify (\es -> es {esProg = k1, esLocation = riName'})
  curRoom <- gets esLocation
  unless (curRoom == secCp) do
    error $ "Failed to get to " <> secCp
  gets \ExplorerState {esGraph, esInventory, esProg} ->
    let [(opDir, _)] = M.toList (esGraph M.! "Pressure-Sensitive Floor")
     in (esInventory, oppositeDir opDir, esProg)

explore :: Explorer ExplorerResult
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
                     (const False)
                     esGraph
                     esUnknowns
                     (S.singleton riName)
                     (Seq.singleton (Just riName, []))
                 case mPlan of
                   Nothing -> do
                     unk <- gets esUnknowns
                     unless (all S.null unk) do
                       error "Unexpected: some unknown areas are unreachable."

                     let debug = False
                     when debug do
                       g <- gets esGraph

                       curLoc <- gets esLocation
                       inv <- gets esInventory
                       liftIO $ do
                         putStrLn $ "Current location: " <> curLoc
                         putStrLn $ "Inventory: " <> intercalate ", " inv
                         putStrLn "Graph:"
                         forM_ (M.toList g) \(k, v) -> do
                           putStrLn k
                           forM_ (M.toList v) $ \(d, dst) -> do
                             putStrLn $ "  " <> show d <> " -> " <> dst
                         putStrLn "Todos:"
                         forM_ (M.toList unk) \(k, v) -> do
                           unless (null v) do
                             putStrLn $ k <> ": " <> intercalate ", " (fmap show $ S.toList v)

                     moveToSecurityCheckpoint
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
               _ -> error $ "Unexpected output structure: " <> show resps
             Nothing ->
               error $ "Parse failure, the output was: " <> show out)
    r0

runExplorer :: IO AsciiResult -> IO ExplorerResult
runExplorer prog =
  evalStateT
    explore
    ExplorerState
      { esGraph = M.empty
      , esLocation = error "unkown"
      , esUnknowns = M.empty
      , esInventory = []
      , esProg = prog
      }
