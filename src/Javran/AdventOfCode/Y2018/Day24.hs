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

module Javran.AdventOfCode.Y2018.Day24
  (
  )
where

{- HLINT ignore -}

import Control.Applicative
import Control.Monad
import Control.Monad.Loops (untilJust)
import Control.Monad.State.Strict
import Data.Char
import Data.Coerce
import Data.Function
import Data.Function.Memoize (memoFix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Javran.AdventOfCode.Prelude
import Text.ParserCombinators.ReadP hiding (count, get, many)
import Text.Printf

data Day24 deriving (Generic)

newtype DamageType = DamageType Int deriving (Show)

newtype ImmuneWeak = ImmuneWeak (IS.IntSet, IS.IntSet) deriving (Show)

data Side = ImmuneSystem | Infection deriving (Eq, Show)

opposite :: Side -> Side
opposite = \case
  ImmuneSystem -> Infection
  Infection -> ImmuneSystem

data ArmyGroup = ArmyGroup
  { agCount :: Int
  , agHp :: Int
  , agImmuneWeak :: ImmuneWeak
  , agAttack :: (Int, DamageType)
  , agInit :: Int
  }
  deriving (Show)

armyGroupP :: ReadP ((String -> DamageType) -> ArmyGroup, S.Set String)
armyGroupP = do
  agCount <- decimal1P <* string " units each with "
  agHp <- decimal1P <* string " hit points "
  let dmgTyP = munch1 isAlpha
      dmgTysP = dmgTyP `sepBy1` string ", "
      immuneWeakP =
        mconcat
          <$> ((immuneToP <++ weakToP) `sepBy1` string "; ")
      immuneToP =
        ((,mempty) . S.fromList) <$> (string "immune to " *> dmgTysP)
      weakToP =
        ((mempty,) . S.fromList) <$> (string "weak to " *> dmgTysP)
  immuneWeak@(immunes, weaks) <-
    between (char '(') (string ") ") immuneWeakP
      <++ pure mempty
  _ <- string "with an attack that does "
  dmg <- decimal1P <* char ' '
  dmgTy <- dmgTyP <* string " damage at initiative "
  agInit <- decimal1P
  let mkArmy sToI =
        ArmyGroup
          { agCount
          , agHp
          , agImmuneWeak =
              let f :: S.Set String -> IS.IntSet
                  f = IS.fromList . coerce . fmap sToI . S.toList
               in ImmuneWeak $ bimap f f immuneWeak
          , agAttack = (dmg, sToI dmgTy)
          , agInit
          }
  pure (mkArmy, S.unions [immunes, weaks, S.singleton dmgTy])

inputP :: ReadP (CombatState, DamageType -> String)
inputP = do
  let nl = void (char '\n')
  _ <- string "Immune System:\n"
  (immuneAgs, acc0) <- unzip <$> many1 (armyGroupP <* nl)
  nl
  _ <- string "Infection:\n"
  (infectionAgs, acc1) <- unzip <$> many1 (armyGroupP <* nl)
  let allDmgTypes = S.toList $ S.unions $ acc0 <> acc1
      dmgTypes = V.fromList allDmgTypes
      revDmgTypes = M.fromList (zip allDmgTypes [0 ..])
      sToI = DamageType . (revDmgTypes M.!)
      iToS (DamageType i) = dmgTypes V.! i
      allArmyGroups =
        fmap (\g -> (ImmuneSystem, g sToI)) immuneAgs
          <> fmap (\g -> (Infection, g sToI)) infectionAgs
  pure
    ( IM.fromListWith (error "initiative conflict") $
        fmap (\p@(_, g) -> (agInit g, p)) allArmyGroups
    , iToS
    )

pprParsed (combat, iToS) = do
  let (xs, ys) =
        bimap (fmap snd) (fmap snd) $
          partition ((== ImmuneSystem) . fst) $
            fmap snd $ IM.toDescList combat
      pprGroup
        ArmyGroup
          { agCount
          , agHp
          , agImmuneWeak =
            ImmuneWeak (im, wk)
          , agAttack = (atkVal, atkTy)
          , agInit = i
          } = do
          printf
            "  Units: %d, Hps: %d, Attack: %d %s, Init: %d\n"
            agCount
            agHp
            atkVal
            (iToS atkTy)
            i
          unless (IS.null im) do
            putStrLn $
              "    Immune to: "
                <> intercalate ", " (fmap (iToS . DamageType) $ IS.toList im)
          unless (IS.null wk) do
            putStrLn $
              "    Weak to: "
                <> intercalate ", " (fmap (iToS . DamageType) $ IS.toList wk)
  putStrLn "Immune system:"
  mapM_ pprGroup xs
  putStrLn "Infection:"
  mapM_ pprGroup ys

effectivePower :: ArmyGroup -> Int
effectivePower ArmyGroup {agCount, agAttack = (dmg, _)} = agCount * dmg

{-
  Looks like it's safe to identify a group by its initiative.
 -}
type CombatState = IM.IntMap (Side, ArmyGroup)

damageFactor :: DamageType -> ImmuneWeak -> Int
damageFactor (DamageType dt) (ImmuneWeak (im, wk))
  | IS.member dt im = 0
  | IS.member dt wk = 2
  | otherwise = 1

{-
  return values:
  - Nothing: cannot deal any damage
  - Just _: can deal damage
 -}
dealDamage :: ArmyGroup -> ArmyGroup -> Maybe Int
dealDamage atkGrp@ArmyGroup {agAttack = (_, atkTy)} ArmyGroup {agImmuneWeak = defImWk} = do
  let damageDealt = effectivePower atkGrp * damageFactor atkTy defImWk
  {-
    Note that it's unclear what to do
    if a positive amount of damage can be dealt,
    but it won't result in any unit loss.
   -}
  damageDealt <$ guard (damageDealt > 0)

selectTarget :: ArmyGroup -> [ArmyGroup] -> Maybe ArmyGroup
selectTarget atkGrp tgts0 = do
  let tgts1 =
        mapMaybe
          (\defGrp ->
             dealDamage atkGrp defGrp >>= \dmgDlt -> Just (dmgDlt, defGrp))
          tgts0
      sortedTgts =
        sortOn
          (\(dmgDlt, defGrp@ArmyGroup {agInit}) ->
             ( Down dmgDlt
             , Down $ effectivePower defGrp
             , Down agInit
             ))
          tgts1
  snd <$> listToMaybe sortedTgts

targetSelectionPhase :: State CombatState [(Int, Maybe Int)]
targetSelectionPhase = do
  allGrps <- get
  let initAtkGrps =
        sortOn
          (\(_side, g) ->
             (Down (effectivePower g), Down (agInit g)))
          (IM.elems allGrps)

  fix
    (\loop atkGrps selectables selected ->
       case atkGrps of
         [] -> pure (sortOn (Down . fst) selected)
         (atkSide, atkGrp) : atkGrps' ->
           let defGrps = concatMap (\(s, g) -> [g | s == opposite atkSide]) $ IM.elems selectables
               sel = selectTarget atkGrp defGrps
               selectables' = case sel of
                 Nothing -> selectables
                 Just ArmyGroup {agInit = i} -> IM.delete i selectables
            in loop atkGrps' selectables' ((agInit atkGrp, agInit <$> sel) : selected))
    initAtkGrps
    allGrps
    []

attack :: ArmyGroup -> ArmyGroup -> Maybe ArmyGroup
attack atkGrp defGrp@ArmyGroup {agCount = defCnt, agHp = defHp} = do
  let Just dmgDealt =
        -- target selection is at fault if we cannot deal any damage.
        dealDamage atkGrp defGrp
      unitLost = dmgDealt `quot` defHp
      agCount' = defCnt - unitLost
  guard $ agCount' > 0
  pure defGrp {agCount = agCount'}

attackingPhase :: [(Int, Maybe Int)] -> State CombatState (Maybe Side)
attackingPhase actions = do
  forM_ actions \(atk, mDef) -> do
    st <- get
    let mAtkDef = do
          attacker <- st IM.!? atk
          defenderInit <- mDef
          defender <- st IM.!? defenderInit
          pure (attacker, defender)
    case mAtkDef of
      Nothing -> pure ()
      Just ((_aSide, atkGrp), (dSide, defGrp)) -> do
        let mDefGrp = attack atkGrp defGrp
        modify (IM.update (\_ -> (dSide,) <$> mDefGrp) (agInit defGrp))
  do
    st <- get
    let (xs, ys) = partition ((== ImmuneSystem) . fst) $ IM.elems st
    pure
      if
          | null xs -> Just Infection
          | null ys -> Just ImmuneSystem
          | otherwise -> Nothing

simulate = untilJust (targetSelectionPhase >>= attackingPhase)

instance Solution Day24 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow} = do
    inp@(st, _) <- consumeOrDie inputP <$> getInputS
    let (r, s) = runState (simulate) st
    pprParsed inp
    print r
    forM_ (IM.elems s) $ \(side, g) -> do
      print (side, agCount g)
    let ans = sum $ fmap (agCount . snd) $ IM.elems s
    answerShow ans
