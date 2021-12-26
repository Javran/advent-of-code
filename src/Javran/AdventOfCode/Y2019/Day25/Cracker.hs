{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2019.Day25.Cracker
  ( runCracker
  )
where

{-
  Cracker assumes that the droid is at Security Checkpoint and
  have all items necessary for cracking and attempts to crack the keypad code.
 -}

import Control.Monad
import Control.Monad.State.Strict
import Data.Char
import Data.List
import qualified Data.Set as S
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2019.Day25.Common
import Javran.AdventOfCode.Y2019.Day25.Explorer
import Javran.AdventOfCode.Y2019.Day25.ResponseParser

type Cracker = StateT CrackerState IO

data CrackerState = CrackerState
  { csSearchSpace :: S.Set (S.Set String) -- current search space
  , csInventory :: S.Set String -- current inventory
  , csProg :: IO AsciiResult
  }

takeOrDropItem :: Bool -> String -> Cracker ()
takeOrDropItem isTake item = do
  p <- gets csProg
  r0 <- liftIO p
  case r0 of
    AsciiNeedCommand k -> do
      AsciiOutput _ k' <- liftIO $ k $ (if isTake then "take " else "drop ") <> item
      modify \cs -> cs {csProg = k'}
    _ -> error "unexpected"

threeWays :: Ord a => S.Set a -> S.Set a -> (S.Set a, S.Set a, S.Set a)
threeWays l r = (S.difference l r, S.intersection l r, S.difference r l)

{-
  Tries to optimize the set to test, this set should evenly spread
  the search space so that we get some performance similar to that of binary search.
  But since Set operations are expensive, we don't want to try all
  possibilities, but just some in the middle.
 -}
optimizeAttempt :: S.Set (S.Set String) -> S.Set String
optimizeAttempt ss = snd $ maximumBy (comparing fst) do
  let sz = S.size ss
      spread :: Double
      spread = 0.4
      inds :: [Int]
      inds =
        [ max 0 (floor $ fromIntegral @_ @Double sz * (0.5 - spread))
        , min (sz -1) (ceiling $ fromIntegral @_ @Double sz * (0.5 + spread))
        ]
  s <- fmap (\i -> S.elemAt i ss) inds
  let (ls, rs) = (S.filter (s `S.isSubsetOf`) ss, S.filter (`S.isSubsetOf` s) ss)
  pure (S.size ls * S.size rs, s)

crack :: Dir -> Cracker Int
crack psfDir = do
  ss <- gets csSearchSpace
  when (null ss) $
    error "search space exhausted"
  let tryInv = optimizeAttempt ss
  curInv <- gets csInventory
  let (itemOldOnly, _, itemNewOnly) = threeWays curInv tryInv
  forM_ (S.toList itemOldOnly) \item ->
    takeOrDropItem False item
  forM_ (S.toList itemNewOnly) \item ->
    takeOrDropItem True item
  modify \cs -> cs {csInventory = tryInv}
  p0 <- gets csProg
  AsciiNeedCommand k0 <- liftIO p0
  AsciiOutput out k1 <- liftIO $ k0 (fmap toLower $ show psfDir)
  modify \cs -> cs {csProg = k1}
  case consumeAllWithReadP responsesP out of
    Just
      [ RespRoomInfo
          RoomInfo
            { riPressureSensitiveExtra =
              Just ParFailure {parFailureShouldBeLighter}
            }
        , _
        ] -> do
        {-
          when parFailureShouldBeLighter is True:
            the expected droid weight is lighter than we are,
            meaning everything containing current inventory cannot be the solution.
         -}
        let shouldExclude =
              if parFailureShouldBeLighter
                then (tryInv `S.isSubsetOf`)
                else (`S.isSubsetOf` tryInv)
        modify \cs ->
          cs
            { csSearchSpace =
                S.filter (not . shouldExclude) $ csSearchSpace cs
            }
        crack psfDir
    Just
      [ RespRoomInfo
          RoomInfo
            { riPressureSensitiveExtra =
              Just (ParSuccess ans)
            }
        ] -> pure ans
    xs -> error $ "unexpected response: " <> show xs

runCracker :: ExplorerResult -> IO Int
runCracker (fullInventory, psfDir, prog) = do
  let fullInv = S.fromList fullInventory
  evalStateT
    (crack psfDir)
    CrackerState
      { csSearchSpace = S.powerSet fullInv
      , csInventory = fullInv
      , csProg = prog
      }
