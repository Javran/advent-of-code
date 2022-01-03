{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Javran.AdventOfCode.Y2018.Day15
  (
  )
where

{-
  (ranting)

  Who the f comes up with this and has the guts to publish this bs,
  please go f yourself, excuse my language but you deserve exactly that.

  Or tell me what exactly am I supposed to learn from this?
  Is this a fcking reading contest?
 -}

import Control.Lens
import Control.Monad
import Control.Monad.Cont
import Control.Monad.State.Strict
import Data.List
import qualified Data.Map.Strict as M
import Data.Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Javran.AdventOfCode.Prelude

data Day15 deriving (Generic)

{-
  Note that for this one *y goes first, then x*
  This is so that we have Ord instance being consistent with "reading order".
 -}
type Coord = (Int, Int)

{-
  storing mapping from a unit to its hitpoint.

  Hitpoints = (<Hitpoints of Elves>, <Hitpoints of Goblins>)
 -}
type HpState = M.Map Coord Int

type Hitpoints = (HpState, HpState)

type Graph = M.Map Coord [Coord]

data Action
  = MoveThenAttack (Maybe Coord) (Maybe Coord)
  | EndCombat
  deriving (Show)

parseFromRaw :: [String] -> (Graph, Hitpoints)
parseFromRaw raw =
  ( g
  , ( M.fromList $ concat elves
    , M.fromList $ concat goblins
    )
  )
  where
    g = M.fromListWith (error "no conflict") do
      coord@(y, x) <- opens
      -- TODO: somehow flatten this gives us the wrong order?
      let coordNext = do
            -- explicit listing to make sure it's in reading order.
            coord' <- [(y -1, x), (y, x -1), (y, x + 1), (y + 1, x)]
            coord' <$ guard (S.member coord' opensSet)
      pure (coord, coordNext)
    (elves, goblins) = unzip combatUnits
    opensSet = S.fromList opens
    (opens, combatUnits) = unzip do
      (y, rs) <- zip [0 ..] raw
      (x, ch) <- zip [0 ..] rs
      guard $ ch `elem` ".EG"
      let coord = (y, x)
          em = [(coord, 200 :: Int) | ch == 'E']
          gm = [(coord, 200 :: Int) | ch == 'G']
      pure (coord, (em, gm))

_pprGame :: Graph -> GameState -> IO ()
_pprGame g GameState {gsHps = (elves, goblins), gsRound} = do
  let Just (MinMax2D ((minY, maxY), (minX, maxX))) =
        foldMap (Just . minMax2D) $ M.keys g
      units :: M.Map Coord (Either Int Int)
      units = M.union (M.map Left elves) (M.map Right goblins)
  putStrLn ("After round " <> show gsRound)
  forM_ [minY - 1 .. maxY + 1] \y -> do
    let render x
          | Just _ <- elves M.!? coord = "E"
          | Just _ <- goblins M.!? coord = "G"
          | Just _ <- g M.!? coord = " "
          | otherwise = "█"
          where
            coord = (y, x)
        rowUnits =
          fmap snd
            . sortOn fst
            . fmap (\((_, x'), hp) -> (x', hp))
            . M.toList
            $ M.filterWithKey (\(y', _) _ -> y' == y) units
        rowExtra = case rowUnits of
          [] -> ""
          _ : _ ->
            " "
              <> intercalate
                ", "
                (fmap
                   (\case
                      Left v -> "E:" <> show v
                      Right v -> "G:" <> show v)
                   rowUnits)
    putStrLn $ concatMap render [minX - 1 .. maxX + 1] <> rowExtra

findPath :: Graph -> (Coord -> Bool) -> S.Set Coord -> Seq.Seq (Coord, [Coord]) -> S.Set Coord -> Maybe [Coord]
findPath g isAvailable goals = fix \loop -> curry \case
  (Seq.Empty, _) -> Nothing
  ((cur, pRev) Seq.:<| q1, discovered) ->
    if S.member cur goals
      then Just (reverse pRev)
      else
        let nexts = do
              coord' <- g M.! cur
              guard $ isAvailable coord' && S.notMember coord' discovered
              pure (coord', coord' : pRev)
            discovered' = foldr S.insert discovered (fmap fst nexts)
            q2 = q1 <> Seq.fromList nexts
         in loop q2 discovered'

-- Computes the action of a unit
unitAction :: Graph -> HpState -> HpState -> Coord -> Action
unitAction graph friends enemies myCoord = either id id do
  when (null enemies) do
    Left EndCombat
  let isAvailable coord =
        M.notMember coord friends && M.notMember coord enemies
      moveDsts = S.filter (\c -> c == myCoord || isAvailable c) $ S.fromList do
        ec <- M.keys enemies
        -- get adjacents of this enemy
        graph M.! ec
      mayAttackFromCoord coord = do
        let possibleTargets = do
              c' <- graph M.! coord
              Just eHp <- pure $ enemies M.!? c'
              pure (eHp, c')
        Min minEnemyHp <- foldMap (Just . Min . fst) possibleTargets
        -- just pick the first one available - this should already be in reading order.
        let (_, target) : _ = filter ((== minEnemyHp) . fst) possibleTargets
        pure target
  Right $ case findPath graph isAvailable moveDsts (Seq.singleton (myCoord, [])) (S.singleton myCoord) of
    Nothing -> MoveThenAttack Nothing Nothing
    Just [] ->
      -- no move necessary, just attack.
      MoveThenAttack Nothing (mayAttackFromCoord myCoord)
    Just (mv : _) ->
      MoveThenAttack (Just mv) (mayAttackFromCoord mv)

data GameState = GameState
  { gsHps :: Hitpoints
  , gsRound :: Int
  }

data CombatResult
  = EndedNormally
  | ElfDied
  deriving (Show, Eq)

performRound :: Monad m => Graph -> Maybe Int -> ContT a (StateT GameState m) (Maybe CombatResult)
performRound g mElfAttackPower = callCC \done -> do
  (elves, goblins) <- gets gsHps
  let (elfAttackPower, haltOnElfDeath) = case mElfAttackPower of
        Nothing -> (3, False)
        Just v -> (v, True)
      iteratee :: M.Map Coord Bool
      iteratee =
        M.unionWithKey
          (\k _ _ -> error $ "duplicated: " <> show k)
          (M.map (const True) elves)
          (M.map (const False) goblins)
  forM_
    (M.toAscList iteratee)
    \(coord, isElf) -> do
      let attackPower = if isElf then elfAttackPower else 3
          _friend = if isElf then _1 else _2
          _enemy = if isElf then _2 else _1
      isAlive <- gets (M.member coord . (^. _friend) . gsHps)
      when isAlive do
        (friends, enemies) <- gets (((,) <$> (^. _friend) <*> (^. _enemy)) . gsHps)
        case unitAction g friends enemies coord of
          EndCombat -> done (Just EndedNormally)
          MoveThenAttack mMoveTarget mAttackTarget -> do
            maybe
              (pure ())
              (\mvTo ->
                 let f m = M.insert mvTo hp $ M.delete coord m
                       where
                         hp = m M.! coord
                  in modify (\s -> s {gsHps = gsHps s & _friend %~ f}))
              mMoveTarget
            maybe
              (pure ())
              (\attackAt -> do
                 let f =
                       M.update
                         (\v -> let v' = v - attackPower in v' <$ guard (v' > 0))
                         attackAt
                 modify (\s -> s {gsHps = gsHps s & _enemy %~ f})
                 unless isElf do
                   -- goblin attacking elf.
                   targetIsAlive <- gets (M.member attackAt . (^. _enemy) . gsHps)
                   when (not targetIsAlive && haltOnElfDeath) do
                     done (Just ElfDied))
              mAttackTarget

  modify (\s -> s {gsRound = gsRound s + 1})
  pure Nothing

simulate :: Graph -> Maybe Int -> GameState -> ((CombatResult, Int), GameState)
simulate g mElfAttack initSt =
  runState (runContT sim pure) initSt
  where
    sim = do
      end <- performRound g mElfAttack
      case end of
        Nothing -> sim
        Just r -> do
          GameState {gsRound, gsHps = (es, gs)} <- get
          pure (r, gsRound * (sum es + sum gs))

instance Solution Day15 where
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    xs <- lines <$> getInputS
    let (g, hps) = parseFromRaw xs
        initSt = GameState {gsHps = hps, gsRound = 0}
    do
      let ((_, outcome), _) = simulate g Nothing initSt
      answerShow outcome
    do
      {-
        A binary search is probably fine, but I won't bet my money
        on this puzzle having a monotonic property that guarantees correctness of
        binary search - the linear approach looks fine and doesn't seem to have a
        terrible performance, that where I'm heading.
       -}
      let simulations =
            fmap (\i -> simulate g (Just i) initSt) [4 ..]
          ((_, outcome), _) : _ =
            dropWhile ((== ElfDied) . fst . fst) simulations
      answerShow outcome
