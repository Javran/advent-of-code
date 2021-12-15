{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Javran.AdventOfCode.Y2019.Day13
  (
  )
where

import Control.Monad
import Control.Monad.State.Strict
import qualified Data.Array.IO as AIO
import qualified Data.Map.Strict as M
import Data.Word
import GHC.Generics (Generic)
import Javran.AdventOfCode.ColorfulTerminal
import Javran.AdventOfCode.Prelude
import Javran.AdventOfCode.Y2019.IntCode

data Day13 deriving (Generic)

showGame :: OutputMethod -> GameState -> IO ()
showGame om GameState {gsScreen, gsScore} = case om of
  OutputForTest {} -> pure ()
  _ -> showGameOnTerm
  where
    showGameOnTerm = do
      let (runTermOut, render) = case om of
            OutputForTest {} -> unreachable
            OutputBasicTerm rto ->
              ( rto
              , \v ->
                  termText $ case v of
                    0 -> "  "
                    1 -> "##"
                    2 -> "[]"
                    3 -> "--"
                    4 -> "()"
                    _ -> error $ "invalid value" <> show v
              )
            OutputColorTerm
              ColorfulTerminal
                { setForeground = withFg
                , runTermOut = rto
                } ->
                ( rto
                , \v ->
                    case v of
                      0 -> termText "  "
                      1 -> withFg White $ termText "██"
                      2 -> withFg Cyan $ termText "░░"
                      3 -> withFg Magenta $ termText "━━"
                      4 -> withFg Yellow $ termText "◖◗"
                      _ -> error $ "invalid value" <> show v
                )
      threadDelay (1000 * 20)
      ((minX, minY), (maxX, maxY)) <- AIO.getBounds gsScreen
      forM_ [minY .. maxY :: Int] $ \y -> do
        let getTile x = AIO.readArray gsScreen (x, y)
        ts <- mapM (fmap render . getTile) [minX .. maxX]
        runTermOut (mconcat ts <> termText "\n")
      putStrLn $
        "Current score: " <> maybe "?" show gsScore

data GameState = GameState
  { gsScreen :: AIO.IOUArray (Int, Int) Word8
  , gsScore :: Maybe Int
  , gsPaddleX :: Maybe Int
  , gsBallX :: Maybe Int
  }

playGame :: OutputMethod -> IO (Result a) -> StateT GameState IO (Maybe Int)
playGame om prog = do
  r <- liftIO prog
  case r of
    Done {} ->
      gets gsScore
    NeedInput k -> do
      GameState {gsPaddleX, gsBallX} <- get
      case om of
        OutputForTest {} -> pure ()
        _ -> get >>= liftIO . showGame om
      let i = case (gsPaddleX, gsBallX) of
            (Just pX, Just bX) -> case compare pX bX of
              LT -> 1
              EQ -> 0
              GT -> -1
            _ -> 0
      playGame om (k i)
    SentOutput {} -> do
      ([x, y, val], k) <- liftIO $ communicate [] 3 (pure r)
      if (x, y) == (-1, 0)
        then modify (\gs -> gs {gsScore = Just val})
        else do
          case val of
            3 ->
              modify (\gs -> gs {gsPaddleX = Just x})
            4 ->
              modify (\gs -> gs {gsBallX = Just x})
            _ -> pure ()
          case om of
            OutputForTest {} -> pure ()
            _ -> do
              -- array update is not necessary for running tests,
              -- as only compare the final result.
              arr <- gets gsScreen
              liftIO $ AIO.writeArray arr (x, y) (fromIntegral val)
      playGame om k

instance Solution Day13 where
  solutionRun _ SolutionContext {getInputS, answerShow, answerS, terminal} = do
    xs <- parseCodeOrDie <$> getInputS
    screenDim <- do
      let prog = startProgramFromFoldable xs
      screen <-
        fix
          (\loop curProg acc -> do
             r <- curProg
             case r of
               Done {} -> pure acc
               NeedInput {} -> errInvalid
               SentOutput {} -> do
                 ([x, y, tileId], k) <- communicate [] 3 (pure r)
                 loop k (M.insert (x, y) tileId acc))
          prog
          M.empty
      let Just (MinMax2D ((minX, maxX), (minY, maxY))) =
            foldMap (Just . minMax2D) $
              M.keys screen
      -- it seems to be a safe assumption that
      -- we can carry screen dimension over to part 2.
      ((minX, maxX), (minY, maxY))
        <$ answerShow (M.size $ M.filter (== 2) screen)
    do
      let ((minX, maxX), (minY, maxY)) = screenDim
      arr <- AIO.newArray @AIO.IOUArray ((minX, minY), (maxX, maxY)) (0 :: Word8)
      let xs' = 2 : tail xs
          prog = startProgramFromFoldable xs'
          initSt =
            GameState
              { gsScreen = arr
              , gsScore = Nothing
              , gsPaddleX = Nothing
              , gsBallX = Nothing
              }
      Just v <- evalStateT (playGame (getOutputMethod answerS terminal) prog) initSt
      answerShow v
