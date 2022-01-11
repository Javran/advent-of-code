{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Javran.AdventOfCode.Y2018.Day24
  (
  )
where

{-
  (ranting)
  I'm not sure what kind of douchebag insists on having another instance
  of this simulation bullshit, after we've done with day 15.
  *What* is the fun in reading a lengthy specification (if you can even call it a spec),
  and implementing it exactly as specified?
  I'd like to speculate that somebody implemented this the first time and discovered
  this "stalemate" situation and decided that this would be a great AoC puzzle,
  which just ends up making us go through all these hassle to get to this realization,
  which won't even much time to deal with.
  So, tell me what exactly are we supposed to have fun with this one, if there's any?
 -}

import Control.Monad
import Control.Monad.Loops
import Control.Monad.State.Strict
import Data.Char
import Data.Coerce
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
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
        (,mempty) . S.fromList <$> (string "immune to " *> dmgTysP)
      weakToP =
        (mempty,) . S.fromList <$> (string "weak to " *> dmgTysP)
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

pprParsed :: (CombatState, DamageType -> [Char]) -> IO ()
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
dealDamage
  atkGrp@ArmyGroup {agAttack = (_, atkTy)}
  ArmyGroup {agImmuneWeak = defImWk} = do
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

targetSelectionPhase :: CombatState -> [(Int, Maybe Int)]
targetSelectionPhase allGrps =
  let initAtkGrps =
        sortOn
          (\(_side, g) ->
             (Down (effectivePower g), Down (agInit g)))
          (IM.elems allGrps)
   in fix
        (\loop atkGrps selectables selected ->
           case atkGrps of
             [] -> sortOn (Down . fst) selected
             (atkSide, atkGrp) : atkGrps' ->
               let defGrps =
                     concatMap
                       (\(s, g) -> [g | s == opposite atkSide])
                       $ IM.elems selectables
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

attackingPhase :: [(Int, Maybe Int)] -> State CombatState (Maybe (Either () (Side, Int)))
attackingPhase actions = do
  stBefore <- get
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
  stAfter <- get
  do
    -- now, to make a decision on whether we want to stop or this fight should continue.
    st <- get
    let (xs, ys) = partition ((== ImmuneSystem) . fst) $ IM.elems st
        summarize :: CombatState -> IM.IntMap (Side, Int, Int)
        summarize = IM.map (\(s, g) -> (s, agCount g, agInit g))
    pure
      if
          | summarize stBefore == summarize stAfter ->
            {-
              it's possible that both armies are too weak to eliminate any unit,
              in which case we should just abort the simulation.
             -}
            Just (Left ())
          | null xs -> Just (Right (Infection, sum $ fmap (agCount . snd) ys))
          | null ys -> Just (Right (ImmuneSystem, sum $ fmap (agCount . snd) xs))
          | otherwise -> Nothing

simulate :: State CombatState (Either () (Side, Int))
simulate = untilJust (gets targetSelectionPhase >>= attackingPhase)

applyBoost :: Int -> CombatState -> CombatState
applyBoost boost =
  IM.map
    (\p@(s, g@ArmyGroup {agAttack = (dmg, dmgTy)}) ->
       if s == Infection
         then p
         else (s, g {agAttack = (dmg + boost, dmgTy)}))

{-
  Find a minimal positive boost that will allow ImmuneSystem to win.
 -}
findMinBoost :: CombatState -> (Int, Int)
findMinBoost initSt =
  case eval 1 of
    Just ans -> (1, ans)
    Nothing ->
      let (lo, hi) = findBound 1 2
       in binSearch lo hi (error "no answer")
  where
    eval boost = case evalState simulate (applyBoost boost initSt) of
      Right (ImmuneSystem, ans) -> Just ans
      _ -> Nothing

    {-
      I don't actually believe that this problem would have a property that
      we can binary search with correctness guarantee on any possible input data,

      So far it seems to be the case for my input and the example,
      and the linear search approach is a bit slow to my liking,
      if we were to include it in our tests - so binary search it is.
     -}
    binSearch l r curAns =
      case eval mid of
        Nothing ->
          if l == mid then curAns else binSearch mid r curAns
        Just ans ->
          if r == mid then (mid, ans) else binSearch l mid (mid, ans)
      where
        mid = (l + r) `quot` 2

    -- assuming `eval lo == Nothing`
    findBound lo hi =
      case eval hi of
        Just _ -> (lo, hi)
        Nothing -> findBound hi (hi * 2)

instance Solution Day24 where
  solutionSolved _ = False
  solutionRun _ SolutionContext {getInputS, answerShow, terminal} = do
    inp@(st, _) <- consumeOrDie inputP <$> getInputS
    when (isJust terminal) do
      pprParsed inp
    do
      let Right (_, ans) = evalState simulate st
      answerShow ans
    do
      let (_, ans) = findMinBoost st
      answerShow ans
