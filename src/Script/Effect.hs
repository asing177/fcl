{-|

Compute what effects an FCL expression has

-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fwarn-unused-binds #-}

module Script.Effect
  ( Effects
  , ScriptEffects(..)
  , EffectError(..)
  , effectCheckScript
  , effectCheckExpr
  , combineSigsEffects
  ) where

import Protolude

import qualified Data.Aeson as A
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Script
import Script.Pretty hiding ((<>))
import Script.Prim
import Script.Typecheck (Sig(..))

newtype Effects = Effects { effectSet :: Set Effect }
  deriving (Show, Eq, Generic, A.FromJSON, A.ToJSON)

data Effect
  = Write PrimOp -- ^ write to ledger state
  | Read PrimOp -- ^  read from ledger state
  | ReadVar Name -- ^ read from contract state
  | WriteVar Name -- ^ write to contract state
  deriving (Show, Eq, Ord, Generic, A.FromJSON, A.ToJSON)

meet :: Effects -> Effects -> Effects
meet (Effects l) (Effects r) = Effects (Set.union l r)

meets :: Foldable t => t Effects -> Effects
meets = foldr meet noEffect

meetsErr :: Foldable t => t (Either [EffectError as ac c] Effects) -> Either [EffectError as ac c] Effects
meetsErr es = case partitionEithers (toList es) of
  ([],effs) -> pure $ meets effs
  (errss,_) -> Left $ mconcat errss

noEffect :: Effects
noEffect = Effects Set.empty

writeVar :: Name -> Effects
writeVar = Effects . Set.singleton . WriteVar

readVar :: Name -> Effects
readVar = Effects . Set.singleton . ReadVar

readEff :: PrimOp -> Effects
readEff = Effects . Set.singleton . Read

writeEff :: PrimOp -> Effects
writeEff = Effects . Set.singleton . Write

data EffectError as ac c
  = LedgerEffectMismatch
    { effectActual :: Effects
    , effectLocation :: Loc
    }
  | PreconditionViolation
    { accessExpected :: Preconditions as ac c
    , accessActual :: Preconditions as ac c
    , violationVar :: Name
    , violationLocation :: Loc
    }
  | HelperEffect
    { helperName     :: Name
    , helperEffects  :: Effects
    , helperLocation :: Loc
    }
  | CollectionEffect
    { collectionEffects  :: Effects
    , collectionLocation :: Loc
    }
  | PreconditionsEffect
    { preconditionExpr     :: LExpr as ac c
    , preconditionName     :: Precondition
    , preconditionLocation :: Loc
    , preconditionEffects  :: Effects
    }
  deriving (Show, Generic, A.FromJSON, A.ToJSON)

instance Pretty Effects where
  ppr (Effects effs) = listOf . Set.toList $ effs

instance Pretty Effect where
  ppr (Read prOp) = "read via" <+> ppr prOp
  ppr (Write prOp) = "write via" <+> ppr prOp
  ppr (ReadVar name) = "read" <+> ppr name
  ppr (WriteVar name) = "write" <+> ppr name

-- Pretty print Sigs with effects for checker output. Assumes that methods
-- can only return `()`.
instance Pretty (Sig, Effects) where
  ppr (Sig args TTransition, effs)
    = tupleOf args <+> "->" <+> ppr (filterEffect useful effs)
    where
      -- Whether we want to display the effect in the checker output
      useful :: Effect -> Bool
      useful = \case
        Write TransitionTo -> False
        _ -> True

      filterEffect :: (Effect -> Bool) -> Effects -> Effects
      filterEffect p = Effects . Set.filter p . effectSet

  ppr _ = panic "Methods can only return TTransition"


instance (Ord as, Ord ac, Ord c, Pretty as, Pretty ac, Pretty c) => Pretty [EffectError as ac c] where
  ppr []
    = "No errors"
  ppr errs@(_:_)
    = vcat . map ppr $ errs

instance (Ord as, Ord ac, Ord c, Pretty as, Pretty ac, Pretty c) => Pretty (EffectError as ac c) where
  ppr LedgerEffectMismatch {..}
    = "Expected no ledger effects"
      <$$> "Actual effects:" <+> ppr effectActual
      <$$> "location:" <+> ppr effectLocation
  ppr PreconditionViolation {..}
    = "Precondition violation:" <+> ppr violationVar <+> "at" <+> ppr violationLocation
      <$$> "Allowed access:" <+> ppr accessExpected
      <$$> "Actual access:" <+> ppr accessActual
  ppr HelperEffect {..}
    = "Helper effect violation:" <+> ppr helperName
      <$$+> "Helper effects:" <+> ppr helperEffects
      <$$+> "location:" <+> ppr helperLocation
  ppr CollectionEffect {..}
    = "Collection effect violation at" <+> ppr collectionLocation
      <$$+> "Collection effects:" <+> ppr collectionEffects
  ppr PreconditionsEffect {..}
    = "Disallowed write effects in" <+> ppr preconditionName <+> "precondition:"
      <$$+> squotes (ppr preconditionExpr) <+> "with effects" <+> squotes (ppr preconditionEffects)
      <$$+> "location:" <+> ppr preconditionLocation

data ScriptEffects
  = ScriptEffects
    { globalEffects :: [(Name, Effects)]
    , methodEffects :: [(Name, Effects)]
    }
  deriving Show

combineSigsEffects :: [(Name,Sig)] -> ScriptEffects -> [(Name,Sig,Effects)]
combineSigsEffects sigs effects
  = mapMaybe lookupEffect sigs
  where
    lookupEffect (name, sig)
      = (name,sig,) <$> List.lookup name (methodEffects effects)

effectCheckScript
  :: forall as ac c. (Eq as, Eq ac, Eq c)
  => Script as ac c
  -> Either [EffectError as ac c] ScriptEffects
effectCheckScript scr
  = case allErrors of
      [] -> pure $ ScriptEffects defEffects
                                 (map (\(x,_,y) -> (x,y)) methodEffects)
      errs@(_:_) -> Left errs
  where
    allErrors :: [EffectError as ac c]
    allErrors = mconcat
      [ defErrors
      , accessErrors
      , methodErrors
      , helperErrors
      ]

    globalVarNms :: [Name]
    globalVarNms = map defnName (scriptDefs scr)

    defErrors :: [EffectError as ac c]
    defEffects :: [(Name, Effects)]
    (defErrors, defEffects)
      = first mconcat
        . Either.partitionEithers
        . map (effectCheckDef globalVarNms)
        . scriptDefs
        $ scr

    accessErrors :: [EffectError as ac c]
    accessErrors
      = flip concatMap methodEffects $ \(methName, precs, effects) ->
          checkDefRolePreconditions preconditionContext precs effects
      where
        preconditionContext = varPreconditions $ scriptDefs scr

    methodErrors :: [EffectError as ac c]
    methodEffects :: [(Name, Preconditions as ac c, Effects)]
    (methodErrors, methodEffects)
      = first mconcat
        . partitionEithers
        . map (\m -> (locVal $ methodName m, methodPreconditions m,) <$> effectCheckMethod globalVarNms m)
        . scriptMethods
        $ scr

    helperErrors :: [EffectError as ac c]
    helperErrors
      = concat
        . lefts
        . map (effectCheckHelper globalVarNms)
        . scriptHelpers
        $ scr

doesOnly
  :: (Effect -> Bool) -- allowed effects
  -> (a -> Either [EffectError as ac c] Effects) -- the check to run
  -> (a -> Effects -> EffectError as ac c) -- make an error if disallowed effects happen
  -> a -- the thing to check
  -> Either [EffectError as ac c] Effects
doesOnly allowed check mkError x = do
  eff <- check x
  let disallowedEffects = Set.filter (not . allowed) . effectSet $ eff
  if null disallowedEffects
    then pure eff
    else Left [mkError x eff]

effectCheckDef
  :: [Name] -> Def as ac c -> Either [EffectError as ac c] (Name, Effects)
effectCheckDef gnms = \case
    GlobalDef _ precs n lexpr -> do
      effectCheckPreconditions gnms precs
      (n,) <$> expectNoLedgerEffects lexpr
    GlobalDefNull _ precs n -> do
      effectCheckPreconditions gnms precs
      pure (locVal n, noEffect)
  where
    expectNoLedgerEffects = doesOnly allowed (effectCheckExpr gnms) mkDefError

    allowed = \case
      Read _ -> False
      Write _ -> False
      _ -> True

    mkDefError expr eff = LedgerEffectMismatch
        { effectActual = eff, effectLocation = located expr }

-- | Enforce global variable role write restrictions
checkDefRolePreconditions
  :: forall as ac c. (Eq as, Eq ac, Eq c)
  => Map Name (Preconditions as ac c) -- ^ mapping of globals to their preconditions
  -> Preconditions as ac c -- ^ method preconditions
  -> Effects -- ^ effect set we are checking against our restriction
  -> [EffectError as ac c]
checkDefRolePreconditions preconditionContext methodPrec
  = mapMaybe (checkVar <=< getWriteVarName) . Set.toList . effectSet
  where
    getWriteVarName :: Effect -> Maybe Name
    getWriteVarName (WriteVar n) = pure n
    getWriteVarName _ = Nothing

    checkVar :: Name -> Maybe (EffectError as ac c)
    checkVar v = case Map.lookup v preconditionContext of
      Just defPrec | not $ methodPrec `subsumes` defPrec ->
        pure $ PreconditionViolation
               { accessExpected = defPrec
               , accessActual = methodPrec
               , violationVar = v
               , violationLocation = NoLoc -- XXX have loc info
               }
      _ -> Nothing

    subsumes :: Preconditions as ac c -> Preconditions as ac c -> Bool
    Preconditions ps1 `subsumes` Preconditions ps2
      = and
          [ subsumesAfter
          , subsumesBefore
          , subsumesRoles
          ]
      where
        get = List.lookup
        subsumesRoles = case (get PrecRoles ps1, get PrecRoles ps2) of
            (_, Nothing) -> True
            (Nothing, Just _) -> False
            (Just (Located _ (ESet rsM)), Just (Located _ (ESet rsV))) -> all (`elem` rsV) rsM
            (Just rsM, Just rsV) -> rsM == rsV -- the FCL expression is possibly not well-typed, however this is not our concern here

        subsumesAfter = case (get PrecAfter ps1, get PrecAfter ps2) of
            (_, Nothing) -> True
            (Nothing, Just _) -> False
            (Just (Located _ (ELit (Located _ (LDateTime dtM)))), Just (Located _ (ELit (Located _ (LDateTime dtV))))) -> dtM >= dtV
            (Just xM, Just xV) -> xM == xV

        subsumesBefore = case (get PrecBefore ps1, get PrecBefore ps2) of
            (_, Nothing) -> True
            (Nothing, Just _) -> False
            (Just (Located _ (ELit (Located _ (LDateTime dtM)))), Just (Located _ (ELit (Located _ (LDateTime dtV))))) -> dtM <= dtV
            (Just xM, Just xV) -> xM == xV

-- | Create mapping from global name to its preconditions
varPreconditions :: forall as ac c. [Def as ac c] -> Map Name (Preconditions as ac c)
varPreconditions = Map.fromList . map defToAssoc
  where
    defToAssoc :: Def as ac c -> (Name, Preconditions as ac c)
    defToAssoc (GlobalDef _ ps n _) = (n, ps)
    defToAssoc (GlobalDefNull _ ps n) = (locVal n, ps)

effectCheckMethod
  :: [Name] -> Method as ac c -> Either [EffectError as ac c] Effects
effectCheckMethod gnms m = meetsErr
    [ effectCheckExpr gnms . methodBody $ m
    , effectCheckPreconditions gnms . methodPreconditions $ m
    ]

effectCheckPreconditions
  :: [Name] -> Preconditions as ac c -> Either [EffectError as ac c] Effects
effectCheckPreconditions gnms (Preconditions ps)
  = meetsErr $ map (effectCheckPrecondition gnms) ps

effectCheckPrecondition
  :: forall as ac c. [Name] -> (Precondition, LExpr as ac c) -> Either [EffectError as ac c] Effects
effectCheckPrecondition gnms (p, e) = case p of
    PrecRoles ->
      doesOnly allowedRoles (effectCheckExpr gnms) (mkPreconditionsErr p) e
    PrecAfter ->
      doesOnly allowedTemporal (effectCheckExpr gnms) (mkPreconditionsErr p) e
    PrecBefore ->
      doesOnly allowedTemporal (effectCheckExpr gnms) (mkPreconditionsErr p) e

  where

    allowedRoles = \case
      ReadVar _ -> True
      Read _ -> True
      _ -> False

    allowedTemporal = \case
      ReadVar _ -> True
      _ -> False

    mkPreconditionsErr :: Precondition -> LExpr as ac c -> Effects -> EffectError as ac c
    mkPreconditionsErr pr expr eff = PreconditionsEffect
      { preconditionExpr = expr
      , preconditionName = pr
      , preconditionLocation = located expr
      , preconditionEffects = eff
      }

-- | Should have no Effects. In the future, we may specific types of effects,
-- like global variable updates, reads from the ledger, or writes to the ledger.
effectCheckHelper
  :: [Name] -> Helper as ac c -> Either [EffectError as ac c] Effects
effectCheckHelper gnms (Helper nm _ body) = go =<< effectCheckExpr gnms body
  where
    go effs
      | effs == noEffect = pure effs
      | otherwise = Left . pure $ HelperEffect (locVal nm) effs (located body)

-- | Effect check an expression. -- If a variable is a global variable, a use of it constitutes a "read" effect
effectCheckExpr
  :: forall as ac c. [Name] -> LExpr as ac c -> Either [EffectError as ac c] Effects
effectCheckExpr gnms expr = case locVal expr of
    ESeq e1 e2 -> effectCheckExprs gnms [e1, e2]
    ELit _ -> pure noEffect
    EVar v
      | locVal v `elem` gnms -> pure $ readVar $ locVal v
      | otherwise -> pure $ noEffect
    EBinOp _ e1 e2 -> effectCheckExprs gnms [e1, e2]
    EUnOp _ s -> effectCheckExpr gnms s
    EIf g e1 e2 -> effectCheckExprs gnms [g, e1, e2]
    ECase s ms -> effectCheckExprs gnms (s : map matchBody ms)
    EBefore g e -> effectCheckExprs gnms [g, e]
    EAfter g e -> effectCheckExprs gnms [g, e]
    EBetween g1 g2 e -> effectCheckExprs gnms [g1, g2, e]
    EAssign name e
      | name `elem` gnms ->
          meetsErr [effectCheckExpr gnms e, pure $ writeVar name]
      | otherwise -> effectCheckExpr gnms e
    ECall (Left primOp) args ->
      meetsErr [pure $ primEffect primOp, effectCheckExprs gnms args]
    ECall (Right _) args -> effectCheckExprs gnms args
    ENoOp -> pure noEffect
    EMap m -> effectCheckCollectionExprs $ Map.keys m <> Map.elems m
    ESet s -> effectCheckCollectionExprs s
    EHole -> panic $ "Hole expression at " <> show (located expr) <> " in `effectCheckExpr`"

  where
    effectCheckCollectionExprs
      :: Foldable t
      => t (LExpr as ac c)
      -> Either [EffectError as ac c] Effects
    effectCheckCollectionExprs
      = doesOnly allowedInCollection (effectCheckExprs gnms) mkCollectionError
      where
        allowedInCollection :: Effect -> Bool
        allowedInCollection = \case
          Read _ -> True
          ReadVar _ -> True
          Write _ -> False
          WriteVar _ -> False

        mkCollectionError _ eff =
          CollectionEffect {collectionEffects = eff, collectionLocation = located expr}

-- | Effect check a bunch of expressions.
effectCheckExprs :: Foldable t => [Name] -> t (LExpr as ac c) -> Either [EffectError as ac c] Effects
effectCheckExprs gnms = meetsErr . map (effectCheckExpr gnms) . toList

primEffect :: PrimOp -> Effects
primEffect p = case p of
  Verify -> readEff p
  Sign -> readEff p
  Block -> readEff p
  Deployer -> readEff p
  Sender -> readEff p
  Created -> readEff p
  Address -> readEff p
  Validator -> readEff p
  Sha256 -> noEffect
  AccountExists -> readEff p
  AssetExists -> readEff p
  ContractExists -> readEff p
  Terminate -> writeEff p
  Now -> readEff p
  TransitionTo -> writeEff p
  Stay -> writeEff p
  CurrentState -> readEff p
  TxHash -> readEff p
  ContractValueExists -> readEff p
  ContractState -> readEff p
  IsBusinessDayUK -> readEff p
  NextBusinessDayUK -> readEff p
  IsBusinessDayNYSE -> readEff p
  NextBusinessDayNYSE -> readEff p
  Between -> noEffect
  TimeDiff -> noEffect
  Fixed1ToFloat -> noEffect
  Fixed2ToFloat -> noEffect
  Fixed3ToFloat -> noEffect
  Fixed4ToFloat -> noEffect
  Fixed5ToFloat -> noEffect
  Fixed6ToFloat -> noEffect
  FloatToFixed1 -> noEffect
  FloatToFixed2 -> noEffect
  FloatToFixed3 -> noEffect
  FloatToFixed4 -> noEffect
  FloatToFixed5 -> noEffect
  FloatToFixed6 -> noEffect
  ContractValue -> readEff p
  AssetPrimOp HolderBalance -> readEff p
  AssetPrimOp TransferTo -> writeEff p
  AssetPrimOp TransferFrom -> writeEff p
  AssetPrimOp CirculateSupply -> writeEff p
  AssetPrimOp TransferHoldings -> writeEff p
  MapPrimOp MapInsert -> noEffect
  MapPrimOp MapDelete -> noEffect
  MapPrimOp MapLookup -> noEffect
  MapPrimOp MapModify -> noEffect
  SetPrimOp SetInsert -> noEffect
  SetPrimOp SetDelete -> noEffect
  CollPrimOp Aggregate -> noEffect
  CollPrimOp Transform -> noEffect
  CollPrimOp Filter -> noEffect
  CollPrimOp Element -> noEffect
  CollPrimOp IsEmpty -> noEffect
