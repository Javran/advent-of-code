{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2021.Day24.Z3
  ( solve
  )
where

{-
  This module solves the problem by using Z3.

  It runs relatively slow comparing with other solutions,
  but it is an interesting idea that I decided to keep this for future reference.
 -}

import Control.Lens hiding (op)
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Writer.CPS
import qualified Data.Text as T
import Javran.AdventOfCode.Prelude
import System.Exit
import Text.Printf
import qualified Text.RawString.QQ as QQ
import qualified Turtle.Line as Turtle
import qualified Turtle.Prelude as Turtle
import qualified Turtle.Shell as Turtle
import qualified Z3.Monad as Z3

genZ3Script :: [(Int, Int, Int)] -> [Int] -> [Int] -> Writer [String] ()
genZ3Script zs lowDs highDs = do
  let p x = tell [x]

  p "(set-logic QF_NIA)"
  forM_ [0 :: Int .. 13] \i -> do
    p $ printf "(declare-const w_%d Int)" i
  forM_ [0 :: Int .. 14] \i -> do
    p $ printf "(declare-const z_%d Int)" i
  p "(assert (= z_0 0))"

  p
    [QQ.r|(define-fun f ((z_in Int) (w Int) (a Int) (b Int) (c Int)) Int
  (let ((x0 (+ (mod z_in 26) b)))
    (let ((x2 (ite (not (= x0 w)) 1 0)))
      (let ((z2 (+ (* (div z_in a)
                      (+ (* 25 x2) 1))
                   (* (+ w c) x2))))
        z2))))|]
  p "(assert (= z_0 0))"
  p "(assert (= z_14 0))"

  forM_ (zip [0 :: Int ..] (zip lowDs highDs)) \(i, (dLo, dHi)) -> do
    p $ printf "(assert (<= %d w_%d %d))" dLo i dHi

  forM_ (zip [0 :: Int ..] zs) $ \(i, (c, d, j)) -> do
    p $ printf "(assert (= (f z_%d w_%d %d %d %d) z_%d))" i i c d j (i + 1)

  p "(check-sat)"
  p "(get-model)"
  p "(exit)"

solveWithZ3 :: [(Int, Int, Int)] -> [Int] -> [Int] -> Z3.Z3 Bool
solveWithZ3 zs lowDs highDs = do
  wVars <- mapM (\i -> Z3.mkFreshIntVar ("w_" <> show i)) [0 :: Int .. 13]
  forM_ (zip [0 :: Int ..] (zip lowDs highDs)) \(i, (dLo, dHi)) -> do
    -- lo <= w_i
    le0 <- (\lo -> Z3.mkLe lo (wVars !! i)) =<< Z3.mkInteger (toInteger dLo)
    le1 <- (\hi -> Z3.mkLe (wVars !! i) hi) =<< Z3.mkInteger (toInteger dHi)
    Z3.assert =<< Z3.mkAnd [le0, le1]

  zVars <- mapM (\i -> Z3.mkFreshIntVar ("z_" <> show i)) [0 :: Int .. 14]
  Z3.assert =<< Z3.mkEq (zVars !! 0) =<< Z3.mkInteger 0
  Z3.assert =<< Z3.mkEq (zVars !! 14) =<< Z3.mkInteger 0

  let mkMystery zVar wVar (a, b, c) = do
        lit0 <- Z3.mkInteger 0
        lit1 <- Z3.mkInteger 1
        x0 <- do
          op0 <- Z3.mkMod zVar =<< Z3.mkInteger 26
          op1 <- Z3.mkInteger (toInteger b)
          Z3.mkAdd [op0, op1]
        x1 <- do
          t <- Z3.mkNot =<< Z3.mkEq x0 wVar
          Z3.mkIte t lit1 lit0
        z1 <- do
          t0 <- do
            t00 <-
              -- z0 `div` a
              Z3.mkDiv zVar =<< Z3.mkInteger (toInteger a)

            t01 <- do
              -- 25 * x1 + 1
              lit25 <- Z3.mkInteger 25
              t010 <- Z3.mkMul [lit25, x1]
              Z3.mkAdd [t010, lit1]
            Z3.mkMul [t00, t01]
          t1 <- do
            -- (w + c) * x1
            t10 <- do
              tc <- Z3.mkInteger (toInteger c)
              Z3.mkAdd [wVar, tc]
            Z3.mkMul [t10, x1]
          Z3.mkAdd [t0, t1]
        pure z1
  forM_ (zip [0 :: Int ..] zs) $ \(i, (a, b, c)) -> do
    lhs <- mkMystery (zVars !! i) (wVars !! i) (a, b, c)
    Z3.assert =<< Z3.mkEq lhs (zVars !! (i + 1))
  r <- Z3.check

  pure $ r == Z3.Sat

checkSat' :: Z3.Z3Env -> [(Int, Int, Int)] -> [Int] -> [Int] -> IO Bool
checkSat' env zs lowDs highDs = do
  r <-
    Z3.evalZ3WithEnv
      (solveWithZ3 zs lowDs highDs)
      env
  pure r

checkSat :: [(Int, Int, Int)] -> [Int] -> [Int] -> IO Bool
checkSat zs lowDs highDs = do
  let scriptContent :: T.Text
      scriptContent = T.pack (unlines $ snd $ runWriter (genZ3Script zs lowDs highDs))
  (ec, _) <- Turtle.procStrict "z3" ["-in"] (Turtle.select $ Turtle.textToLines scriptContent)
  pure $ ec == ExitSuccess

narrowNth :: [(Int, Int, Int)] -> Bool -> Int -> StateT ([Int], [Int]) IO ()
narrowNth zs findMax i = do
  (lowDs, highDs) <- get
  let useBinding = False
  mEnv <-
    if useBinding
      then Just <$> liftIO (Z3.newEnv (Just Z3.QF_NIA) Z3.stdOpts)
      else pure Nothing
  let curCheckSat = maybe checkSat checkSat' mEnv
      binSearch l r mAns =
        if l <= r
          then do
            let mid = (l + r) `quot` 2
            if findMax
              then do
                sat <- curCheckSat zs (lowDs & ix i .~ mid) highDs
                if sat
                  then binSearch (mid + 1) r (Just mid)
                  else binSearch l (mid -1) mAns
              else do
                sat <- curCheckSat zs lowDs (highDs & ix i .~ mid)
                if sat
                  then binSearch l (mid -1) (Just mid)
                  else binSearch (mid + 1) r mAns
          else pure mAns
  Just l' <- liftIO $ binSearch (lowDs !! i) (highDs !! i) Nothing
  modify ((if findMax then first else second) (& ix i .~ l'))

solve :: [(Int, Int, Int)] -> IO (Int, Int)
solve params = do
  let initSt = (replicate 14 1, replicate 14 9)
  -- part 1
  (lowDs, _) <-
    execStateT (forM_ [0 .. 13] \i -> narrowNth params True i) initSt
  -- part 2
  (_, highDs) <-
    execStateT (forM_ [0 .. 13] \i -> narrowNth params False i) initSt
  pure (digitsToInt lowDs, digitsToInt highDs)
