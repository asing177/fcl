{-|

Typechecker and elaboration for FCL langauge.

-}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Script.Typecheck (
  -- ** Types
  TypeError(..),
  TypeErrInfo(..),
  InferState(..),
  InferM,

  -- ** Signatures
  Sig(..),

  -- ** Typechecker
  tcLExpr,
  signatures,
  methodSig,
  functionSig,
  tcMethodCall,
  runInferM,
  tcDefns,
  tcDefn,
  emptyInferState,
  tcPreconditions,
  runSolverM,

  -- ** Pretty Printing
  ppSig,
  ppError,
) where

import Protolude hiding (Type, TypeError, Constraint)
import Unsafe (unsafeIndex)
import Fixed
import Script
import Script.Prim
import Script.Pretty hiding ((<>))
import Utils ((?), duplicates, zipWith3M_)
-- import Control.Exception (assert)
import Control.Monad.State.Strict (modify')

import qualified Data.Aeson as A
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Serialize (Serialize)
import qualified Data.Set as Set
import qualified Data.Map as Map

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Type signature
data Sig = Sig [Type] Type
  deriving (Eq, Show, Generic, Serialize, A.FromJSON, A.ToJSON)

-- | Type error metadata
-- This type uses ByteString for ad-hoc error messages so that we can get a
-- Serialize instance for free.
data TypeErrInfo as ac c
  = UnboundVariable Name                -- ^ Unbound variables
  | Shadow Name TMeta (TypeInfo as ac c)          -- ^ Shadowing variables/methods/helpers
  | InvalidDefinition Name (Expr as ac c) Type Type -- ^ Invalid definition
  | InvalidUnOp UnOp Type               -- ^ Invalid unary op
  | InvalidBinOp BinOp Type Type        -- ^ Invalid binary op
  | UndefinedFunction Name              -- ^ Invocation of non-existent non-primop function
  | InvalidAddress Text                 -- ^ Invalid address
  | InvalidArgType Name Type Type       -- ^ Invalid argument to Method call
  | VarNotFunction Name Type         -- ^ Helpers must be of type 'TFun'
  | ArityFail Name Int Int              -- ^ Incorrect # args supplied to function
  | UnificationFail (TypeInfo as ac c) (TypeInfo as ac c)   -- ^ Unification fail
  | TransitionOnlyInTrueBranch (LExpr as ac c)    -- ^ Transition in an @else@less @if@ statement
  | TransitionOnlyInOneBranch           -- ^ Transition only in one branch of @if@ statement
  | UnreachableStatement Loc            -- ^ The following statement is unreachable because of a transition
  | ExpectedStatement Type              -- ^ Expected statement but got expression
  | MethodUnspecifiedTransition Name    -- ^ Method doesn't specify where to transition to
  | CaseOnNotEnum (TypeInfo as ac c)              -- ^ Case analysis on non-enum type
  | UnknownConstructor EnumConstr       -- ^ Reference to undefined constructor
  | UnknownEnum Name                    -- ^ Reference to unknown enum type
  | EmptyMatches                        -- ^ Case statement with empty matches
  | PatternMatchError
    { patMatchErrorMissing :: [EnumConstr]
    , patMatchErrorDuplicate :: [EnumConstr]
    }                                   -- ^ Pattern match failures
  | Impossible Text -- ^ Malformed syntax, impossible
  | InvalidAccessRestriction (LExpr as ac c) Type -- ^ Role precondition is not valid
  | UnknownHoleInProgram
    -- ^ There is an underconstrained hole, e.g. @z = ?@ where @z@ is a temp variable
  | HoleInProgram
    { inferredType :: Type, suggestions :: [(Name, (TMeta, TypeInfo as ac c))] }
    -- ^ There is a hole in the program, so we give the context
  deriving (Eq, Show, Generic, Serialize, A.FromJSON, A.ToJSON)

-- | Type error
data TypeError as ac c = TypeError
  { errInfo :: TypeErrInfo as ac c
  , errLoc  :: Loc
  } deriving (Eq, Show, Generic, Serialize, A.FromJSON, A.ToJSON)

instance (Ord as, Ord ac, Ord c) => Ord (TypeError as ac c) where
  compare te1 te2 = compare (errLoc te1) (errLoc te2)

-- | Source of type
--
-- TODO Since there are functions (helpers) defined now, error messages could be
-- improved by attaching location information to arguments of functions. This
-- way, when a type origin is assigned to the type of an argument to a function,
-- the origin will point directly to the column the argument name is introduced,
-- instead of the column that the helper function name occurs in.
data TypeOrigin as ac c
  = OutOfThinAir
  | ExpectedFromMethodBody -- ^ Expected type from method body (i.e. @TTransition@)
  | ExpectedFromSeq -- ^ Expected type of left-hand side of @ESeq@
  | VariableDefn Name     -- ^ Top level definitions
  | InferredFromVar Name  -- ^ Local method variable assignment
  | InferredFromExpr (Expr as ac c) -- ^ Local method variable assignment
  | InferredFromLit (Lit as ac c)   -- ^ Literal types
  | InferredFromAssetType Name Type -- ^ Holdings type inferred from asset type passed to primop
  | InferredFromHelperDef Name -- ^ Inferred from helper function definition
  | InferredFromCollType Name TCollection -- ^ Inferred from collection type in collection primop
  | InferredFromAssignment Name -- ^ Inferred from assignment to existing variable
  | InferredFromMethodBody -- ^ Inferred from method body of given method
  | UnaryOperator UnOp    -- ^ From unary operation
  | BinaryOperator BinOp  -- ^ From binary operation
  | IfCondition           -- ^ From if condition
  | DateTimeGuardPred     -- ^ From DateTime guard predicate
  | DateTimeGuardBody     -- ^ From DateTime guard body
  | Assignment            -- ^ From var assignment
  | FunctionArg Int Name  -- ^ Expr passed as function argument + it's position
  | FunctionRet Name      -- ^ Returned from prip op
  | CasePattern Name      -- ^ Enum type of pattern
  | CaseBody (Expr as ac c)         -- ^ Body of case match
  | EmptyCollection       -- ^ From an empty collection
  | MapExpr               -- ^ From a Map expression
  | SetExpr               -- ^ From a Set expression
  | FromPrecondition Precondition -- ^ From a method precondition
  | FromHole
  | EmptyBlock -- ^ An empty block has type @TVoid@
  deriving (Eq, Ord, Show, Generic, Serialize, A.FromJSON, A.ToJSON)

-- | Type error metadata
data TypeInfo as ac c = TypeInfo
  { ttype :: Type        -- ^ What type
  , torig :: TypeOrigin as ac c  -- ^ Where did it come from
  , tloc  :: Loc         -- ^ Where is it located
  } deriving (Show, Eq, Ord, Generic, Serialize, A.FromJSON, A.ToJSON)

tIntInfo, tFloatInfo, tContractInfo, tBoolInfo, tAccountInfo, tDatetimeInfo, tDeltaInfo, tMsgInfo :: TypeOrigin as ac c -> Loc -> TypeInfo as ac c
tIntInfo = TypeInfo TInt
tFloatInfo = TypeInfo TFloat
tContractInfo = TypeInfo TContract
tBoolInfo = TypeInfo TBool
tAccountInfo = TypeInfo TAccount
tDatetimeInfo = TypeInfo TDateTime
tDeltaInfo = TypeInfo TTimeDelta
tMsgInfo = TypeInfo TText

tAssetInfo :: TAsset -> TypeOrigin as ac c -> Loc -> TypeInfo as ac c
tAssetInfo ta = TypeInfo (TAsset ta)

tFixedInfo :: PrecN -> TypeOrigin as ac c -> Loc -> TypeInfo as ac c
tFixedInfo p = TypeInfo (TFixed p)

tFunInfo :: [Type] -> TVar -> TypeOrigin as ac c -> Loc -> TypeInfo as ac c
tFunInfo argTypes tv = TypeInfo (TFun argTypes (TVar tv))

throwErrInferM :: TypeErrInfo as ac c -> Loc -> InferM as ac c (TypeInfo as ac c)
throwErrInferM tErrInfo loc = do
    modify' $ \inferState ->
      inferState { errs = errs inferState ++ [typeError] }
    return $ TypeInfo TError OutOfThinAir loc
  where
    typeError = TypeError tErrInfo loc

throwTypeErrInferM :: TypeError as ac c -> InferM as ac c (TypeInfo as ac c)
throwTypeErrInferM (TypeError err loc) = throwErrInferM err loc

data Hole as ac c = Hole
  { holeLocation :: Loc
  , holeContext  :: Context (TMeta, TypeInfo as ac c)
  , holeTyVar    :: TVar
  } deriving (Eq, Ord, Show)


-- | Type inference monad state
data InferState as ac c = InferState
  { count       :: Int            -- ^ unique counter for variable freshening
  , errs        :: [TypeError as ac c]    -- ^ accumulator for type errors
  , constraints :: [Constraint as ac c]   -- ^ accumulator for type equality constraints
  , holes       :: Set (Hole as ac c)       -- ^ accumulator for holes
  , context     :: Context (TMeta, TypeInfo as ac c)  -- ^ variable context
  } deriving (Show)

emptyInferState :: (Ord as, Ord ac, Ord c) => InferState as ac c
emptyInferState = InferState 0 mempty mempty mempty emptyContext

data TMeta = Global | Temp | FuncArg | HelperFunc
  deriving (Show, Eq, Ord, Generic, Serialize, A.FromJSON, A.ToJSON)

instance Pretty TMeta where
  ppr = \case
    Global -> "global variable"
    Temp -> "temporary variable"
    FuncArg -> "method argument"
    HelperFunc -> "helper function"
--------------------------------------------------------------------------------
-- Functions for manipulating scoped type environments
--------------------------------------------------------------------------------

{- | A context is a non-empty stack of bindings (mappings from names to @a@s),
containing at least the empty map. When we "go into" a child scope, e.g. a
branch of an `if` statement, we push a fresh map onto the stack and pop it when
we are done. To look up the @a@ associated with the binding, we successively
check in the maps until we find the first match or fail when we reach the end.
Currently we don't allow shadowing for typing contexts ('extendContextInferM').
-}
newtype Context a = Context
  { unContext :: NonEmpty (Map Name a) }
  deriving (Eq, Ord, Show)

-- | Empty context
emptyContext :: Context a
emptyContext = Context $ mempty :| []

-- | Variable lookup
lookupContext :: Name -> Context a -> Maybe a
lookupContext v = foldr mplus Nothing . map (Map.lookup v) . unContext

-- | Extend the context with a new binding in the current scope. Ensure that
-- there is no shadowing.
extendContextInferM :: (Name, TMeta, TypeInfo as ac c) -> InferM as ac c ()
extendContextInferM (id, meta, info) = do
  shadowed <- lookupContext id <$> gets context
  case shadowed of
    Nothing ->
      modify' $ \s -> s { context = extendContext id (meta, info) (context s) }
    Just (metaSh, infoSh) ->
      void $ throwErrInferM (Shadow id metaSh infoSh) (tloc info)
  where
    -- | Extend a context with a new--possibly shadowing--binding.
    extendContext :: Name -> a -> Context a -> Context a
    extendContext id v (Context (c :| cs)) = Context (Map.insert id v c :| cs)

-- | Execute an `InferM` computation in a new temporary variable scope, e.g.
-- in one of the branches of an if statement.
bracketScopeTmpEnvM :: InferM as ac c a -> InferM as ac c a
bracketScopeTmpEnvM thingToDo = do
    pushScope
    result <- thingToDo
    popScope
    pure result
  where
    pushScope = modify' $ \s ->
      s { context = Context (mempty :| toList (unContext (context s))) }
    popScope = modify' $ \s ->
      s { context = case (NonEmpty.tail . unContext . context) s of
                        (parent : envs) -> Context (parent :| envs)
                        x -> panic "Bug in type checker: every pop should have had a matching push"
        }

--------------------------------------------------------------------------------


type InferM as ac c = ReaderT EnumInfo (State (InferState as ac c))

runInferM
  :: EnumInfo -> InferState as ac c -> InferM as ac c a -> (a, InferState as ac c)
runInferM enumInfo inferState act
  = runState (runReaderT act enumInfo) inferState

-------------------------------------------------------------------------------
-- Type Signatures for Methods
-------------------------------------------------------------------------------

-- | Typechecks whether the values supplied as arguments to the method
-- call match the method argument types expected
tcMethodCall
  :: forall as ac c. (Show as, Show ac, Show c)
  => EnumInfo -> Method as ac c -> [Value as ac c] -> Either (TypeErrInfo as ac c) ()
tcMethodCall enumInfo method argVals
  = do
  actualTypes <- mapM valueType argVals
  zipWithM_ validateTypes expectedTypes actualTypes
  where
    valueType :: Value as ac c -> Either (TypeErrInfo as ac c) Type
    valueType val
      = case (val, mapType enumInfo val) of
          (_, Just ty) -> pure ty
          (VEnum c, Nothing) -> Left $ UnknownConstructor c
          (o, Nothing) -> Left $ Impossible $ "Malformed value: " <> show o

    expectedTypes = Script.argtys' method

    validateTypes expected actual =
      when (not $ validMethodArgType expected actual) $
        Left $ InvalidArgType (locVal $ methodName method) expected actual

    validMethodArgType :: Type -> Type -> Bool
    validMethodArgType (TAsset _) TAssetAny = True
    validMethodArgType TAssetAny (TAsset _) = True
    validMethodArgType t1 t2 = t1 == t2

signatures :: (Eq as, Eq ac, Eq c, Ord as, Ord ac, Ord c) => Script as ac c -> Either (NonEmpty (TypeError as ac c)) [(Name,Sig)]
signatures (Script enums defns graph methods helpers) =
    case tcErrs of
      []   -> Right methodSigs
      es:ess -> Left . NonEmpty.nub . sconcat $ es:|ess
  where
    enumInfo = createEnumInfo enums
    inferState = snd . runInferM enumInfo emptyInferState $
        tcDefns defns >> tcHelpers helpers

    (tcErrs, methodSigs) =
      partitionEithers $ map (methodSig enumInfo inferState) methods

-- | Typechecks a top-level 'Method' function body and returns a type signature
-- This type 'Sig' differs from helper functions because there is a distinct
-- difference between helper functions and methods. Methods are *not* callable
-- from other methods or functions, whereas helper functions are able to be
-- called from any method or other helper functions.
methodSig
  :: (Ord as, Ord ac, Ord c)
  => EnumInfo
  -> InferState as ac c
  -> Method as ac c
  -> Either (NonEmpty (TypeError as ac c)) (Name, Sig)
methodSig enumInfo initInferState m@(Method _ precs lMethNm args body) =
    case runSolverM resInferState of
      Right _ -> Right (locVal lMethNm, sig)
      Left es -> Left es
  where
    -- Typecheck body of method, generating constraints along the way
    (sig, resInferState) = runInferM enumInfo initInferState $ do
        tcPreconditions precs
        sig@(Sig _argTys typ) <- functionSig (locVal lMethNm, args, body)
        unless (typ == TTransition || typ == TError)
              (void $ throwErrInferM (MethodUnspecifiedTransition (locVal lMethNm)) (located body))
        pure sig

tcHelpers :: (Ord as, Ord ac, Ord c) => [Helper as ac c] -> InferM as ac c ()
tcHelpers helpers =
  forM_ helpers $ \helper -> do
    helperInfo <- tcHelper helper
    extendContextInferM (locVal (helperName helper), HelperFunc, helperInfo)

-- | Typechecks a 'Helper' function body and returns a TypeInfo of TFun,
-- representing the type of a function. This differs from the 'Sig' value
-- returned, because helper functions are injected into the variable environment
-- as variables with 'TFun' types.
tcHelper
  :: (Ord as, Ord ac, Ord c)
  => Helper as ac c
  -> InferM as ac c (TypeInfo as ac c)
tcHelper (Helper fnm args body) = do
  (Sig argTypes retType) <- functionSig (locVal fnm, args, body)
  let tfun = TFun argTypes retType
  pure (TypeInfo tfun (InferredFromHelperDef (locVal fnm)) (located fnm))

-- | Gives a function signature to function-esque tuples:
-- Given a triple of a function name, a list of arguments and an expression
-- find the type signature of the expression body (using some initial state)
functionSig
  :: (Ord as, Ord ac, Ord c)
  => (Name, [Arg], LExpr as ac c)
  -> InferM as ac c Sig
functionSig (fnm, args, body) = do
  argInfos <- zipWithM (tcArg fnm) args [1..]
  bracketScopeTmpEnvM $ do
    mapM_ extendContextInferM argInfos
    retType <- tcLExprScoped body
    pure $ Sig (map argType args) (ttype retType)

-- | Typechecks the argument and returns the type info of the function argument
-- such that it can be added to the typing env in the manner the caller prefers.
tcArg :: forall as ac c. Name -> Arg -> Int -> InferM as ac c (Name, TMeta, TypeInfo as ac c)
tcArg fnm (Arg typ (Located loc anm)) argPos =
    case typ of
      TEnum enm -> do
        enumDoesExist <- enumExists enm
        if enumDoesExist
           then pure argInfo
           else do
             terrInfo <- throwErrInferM (UnknownEnum enm) loc
             pure (anm, FuncArg, terrInfo)
      _ -> pure argInfo
  where
    argInfo = (anm, FuncArg, TypeInfo typ (FunctionArg argPos fnm) loc)

    enumExists :: Name -> InferM as ac c Bool
    enumExists enumNm = do
      enumConstrs <- enumToConstrs <$> ask
      case Map.lookup enumNm enumConstrs of
        Nothing -> pure False
        Just _  -> pure True

-------------------------------------------------------------------------------
-- Typechecker (w/ inference)
-------------------------------------------------------------------------------

tcDefn
  :: forall as ac c. (Ord as, Ord ac, Ord c)
  => Def as ac c -> InferM as ac c ()
tcDefn def = extendContextInferM =<< case def of
  -- GlobalDef variable can be any type
  GlobalDefNull typ precs lnm -> do
    tcPreconditions precs
    let Located loc nm = lnm
        typeInfo = TypeInfo typ (VariableDefn nm) loc
    return (nm, Global, typeInfo)

  GlobalDef typ precs nm lexpr -> do
    tcPreconditions precs
    exprTypeInfo@(TypeInfo _ _ loc) <- tcLExpr lexpr
    let typeInfo = TypeInfo typ (VariableDefn nm) loc
    case unifyDef nm lexpr typeInfo exprTypeInfo of
      Left terr -> void $ throwErrInferM terr loc
      Right _   -> pure ()
    return (nm, Global, typeInfo)

  where
    -- Check if the stated definition type and the rhs expr type match
    unifyDef :: Name -> LExpr as ac c -> TypeInfo as ac c -> TypeInfo as ac c -> Either (TypeErrInfo as ac c) ()
    unifyDef nm le t1 t2 = do
      case runSolverM emptyInferState{ constraints = [Constraint (Just le) t1 t2] } of
        Right _ -> pure ()
        Left (terr :| _) ->
          case errInfo terr of
            UnificationFail ti1 ti2 -> Left $ InvalidDefinition nm (locVal le) (ttype ti1) (ttype ti2)
            otherwise               -> panic "Solver should fail with UnificationFail"


tcDefns :: (Ord as, Ord ac, Ord c) => [Def as ac c] -> InferM as ac c ()
tcDefns = mapM_ tcDefn

-- | Type check an LExpr. Remember to use `bracketScopeTmpEnvM` if temporary
-- variables are meant to be discarded.
tcLExpr
  :: forall as ac c. (Ord as, Ord ac, Ord c, Eq as, Eq ac, Eq c)
  => LExpr as ac c -> InferM as ac c (TypeInfo as ac c)
tcLExpr le@(Located loc expr) = case expr of
  ENoOp     -> return $ TypeInfo TVoid EmptyBlock loc
  EVar nm   -> snd <$> lookupVarType' nm
  EHole     -> do
      -- get the hole context
      ctx <- gets context
      -- create a fresh type variable for the return type
      v <- freshTVar'
      -- add the hole to the infer state
      let h = Hole{ holeLocation = loc, holeContext = ctx, holeTyVar = v }
      modify' $ \s -> s{ holes = Set.insert h (holes s)}
      -- we just return a type variable
      pure TypeInfo{ ttype = TVar v, torig = FromHole, tloc = loc }
  ELit llit -> tcLLit llit
  EBinOp nm e1 e2 -> tcBinOp nm e1 e2
  EUnOp nm e -> tcUnOp nm e
  ESeq e1 e2 -> tcLExpr e1 >>= \case
      TypeInfo TTransition _ _ -> throwErrInferM (UnreachableStatement (located e2)) loc
      t1 -> if isPotentiallyStatement t1
        then do
          addConstr e1 (TypeInfo TVoid ExpectedFromSeq (located e1)) t1
          tcLExpr e2
        else throwErrInferM (ExpectedStatement (ttype t1)) loc
    where
      isPotentiallyStatement typeInfo = case ttype typeInfo of
        TVoid  -> True -- proper statement
        TError -> True -- error, so ignore
        TVar _ -> True -- poly, so will get resolved by constraint
        TAny   -> True -- I don't think this can occur (@buggymcbugfix)
        _      -> False
  ECall mnm argExprs ->
    case mnm of
      Left primOp    -> tcPrim le loc primOp argExprs
      Right helperNm -> do
        let hnm = locVal helperNm
        mTypeInfo <- lookupVarType helperNm
        case mTypeInfo of
          Nothing -> throwErrInferM (UndefinedFunction hnm) loc
          Just (_, tinfo) ->
            case tinfo of
            -- If a TFun type, generate constraints for all the arguments the
            -- function is applied to, and type the whole expression as the
            -- function's return type.
              TypeInfo (TFun argTypes retType) _ _  -> do
                argTypeInfos <- mapM tcLExpr argExprs
                let mkArgTypeInfo ty n = TypeInfo ty (FunctionArg n hnm) loc
                    argTypeInfos' = zipWith mkArgTypeInfo argTypes [1..]
                zipWith3M_ addConstr argExprs argTypeInfos' argTypeInfos
                pure $ TypeInfo retType (FunctionRet hnm) loc
              -- If error typechecking helper, leave it be
              TypeInfo TError _ _ -> pure tinfo
              -- If any other type, report the error but continue
              TypeInfo varType _ _  -> do
                let terr = VarNotFunction (locVal helperNm) varType
                throwErrInferM terr (located helperNm)

  EAssign nm e -> do
    lookupVarType (Located loc nm) >>= \case

      Nothing -> do -- New temp variable, instantiate it
        eTypeInfo@(TypeInfo eType _ eLoc) <- tcLExpr e
        let typeInfo = TypeInfo eType (InferredFromExpr $ locVal e) eLoc
        extendContextInferM (nm, Temp, typeInfo)

      Just (meta, varTypeInfo) -> if meta == Global || meta == Temp
        then do -- can only assign to global or temp variable, so need to check this here
          tvar <- freshTVar
          let retTypeInfo = TypeInfo tvar (InferredFromAssignment nm) (located e)
          addConstr e varTypeInfo retTypeInfo
          eTypeInfo@(TypeInfo eType _ eLoc) <- tcLExpr e
          addConstr e retTypeInfo eTypeInfo
        else void $ throwErrInferM (Shadow nm meta varTypeInfo) loc

    return $ TypeInfo TVoid Assignment (located e)

  EBefore edt e -> do
    dtTypeInfo <- tcLExpr edt
    eTypeInfo  <- tcLExprScoped e
    addConstr edt (tDatetimeInfo DateTimeGuardPred loc) dtTypeInfo
    addConstr e (TypeInfo TVoid DateTimeGuardBody loc) eTypeInfo
    return $ TypeInfo TVoid DateTimeGuardBody loc

  EAfter edt e -> do
    dtTypeInfo <- tcLExpr edt
    eTypeInfo  <- tcLExprScoped e
    addConstr edt (tDatetimeInfo DateTimeGuardPred loc) dtTypeInfo
    addConstr e (TypeInfo TVoid DateTimeGuardBody loc) eTypeInfo
    return $ TypeInfo TVoid DateTimeGuardBody loc

  EBetween startDte endDte e -> do
    startTypeInfo <- tcLExpr startDte
    endTypeInfo <- tcLExpr endDte

    let dtInfo = tDatetimeInfo DateTimeGuardPred loc
    addConstr startDte dtInfo startTypeInfo
    addConstr endDte dtInfo endTypeInfo

    eTypeInfo  <- tcLExpr e
    addConstr e (TypeInfo TVoid DateTimeGuardBody loc) eTypeInfo

    return $ TypeInfo TVoid Assignment loc

  EIf cond e1 e2 -> do
    cTypeInfo  <- tcLExpr cond
    e1TypeInfo <- tcLExprScoped e1
    e2TypeInfo <- tcLExprScoped e2
    let cTypeInfo' = TypeInfo
          { ttype = TBool
          , torig = IfCondition
          , tloc  = tloc cTypeInfo
          }
    addConstr cond cTypeInfo' cTypeInfo
    if (ttype e1TypeInfo == TTransition) `xor` (ttype e2TypeInfo == TTransition)
      then if locVal e2 == ENoOp
        then throwErrInferM (TransitionOnlyInTrueBranch cond) loc
        else throwErrInferM TransitionOnlyInOneBranch loc
      else do
        addConstr e1 e1TypeInfo e2TypeInfo
        let retTypeInfo = TypeInfo
              { ttype = ttype e1TypeInfo
              , torig = IfCondition
              , tloc  = loc
              }
        return retTypeInfo

  ECase scrut ms -> do
    dTypeInfo <- tcLExpr scrut

    tcCasePatterns scrut dTypeInfo . map matchPat $ ms

    -- Associate every body with its type info
    let attachInfo (Match _ bodyExpr) = (bodyExpr,) <$> tcLExprScoped bodyExpr
    bodiesTypeInfos <- mapM attachInfo ms
    -- Add a constraint for every body to have the same type as the
    -- first body
    case bodiesTypeInfos of
      [] -> throwErrInferM EmptyMatches $ located scrut
      ((bodyExpr, tyInfo):eis) -> do
        mapM_ (addConstr bodyExpr tyInfo . snd) eis
        return TypeInfo
          { ttype = ttype tyInfo
          , torig = CaseBody (locVal bodyExpr)
          , tloc  = located bodyExpr
          }

  EMap m -> do
    TypeInfo kTy _ _ <- tcLExprs $ Map.keys m
    TypeInfo vTy _ _ <- tcLExprs $ Map.elems m
    pure $ TypeInfo (TColl (TMap kTy vTy)) MapExpr loc

  ESet s -> do
    TypeInfo ty _ _ <- tcLExprs s
    pure $ TypeInfo (TColl (TSet ty)) SetExpr loc

  where
    tcLExprs :: Foldable t => t (LExpr as ac c) -> InferM as ac c (TypeInfo as ac c)
    tcLExprs = foldM step zero
      where
        step prevTyInfo e = do
          tyInfo <- tcLExpr e
          addConstr e tyInfo prevTyInfo
          pure tyInfo
        zero = TypeInfo TAny EmptyCollection loc

-- | Type check an LExpr in a new scope which doesn't leak temporary variables.
tcLExprScoped :: (Ord as, Ord ac, Ord c) => LExpr as ac c -> InferM as ac c (TypeInfo as ac c)
tcLExprScoped = bracketScopeTmpEnvM . tcLExpr

tcCasePatterns
  :: LExpr as ac c
  -> TypeInfo as ac c
  -> [LPattern]
  -> InferM as ac c ()
tcCasePatterns scrut scrutInfo []
  = void
    . throwErrInferM EmptyMatches
    $ (located scrut)
tcCasePatterns scrut scrutInfo ps@(_:_)
  = case ttype scrutInfo of
      -- Scrutinee of enum type e
      (TEnum e) -> do
        maybeAllConstrs <- Map.lookup e . enumToConstrs <$> ask
        case maybeAllConstrs of
          Nothing
            -> void $ throwErrInferM (UnknownEnum e) topLoc

          Just allConstrs
            -> case (missing allConstrs ps, overlap ps) of
                 ([],[])
                   -> do
                   enumConstrs <- constrToEnum <$> ask
                   mapM_ (addConstr scrut scrutInfo <=< patternInfo enumConstrs) ps

                 (misses, overlaps)
                   -> void $ throwErrInferM (PatternMatchError misses overlaps) topLoc
      _ -> void $ throwErrInferM (CaseOnNotEnum scrutInfo) topLoc
  where
    topLoc = located scrut

    patConstr (Located _ (PatLit c)) = c
    missing allConstrs ps = allConstrs List.\\ map patConstr ps
    overlap ps = duplicates (map patConstr ps)

    patternInfo enumConstrs (Located ploc (PatLit c))
      = case Map.lookup c enumConstrs of
          Nothing
            -> throwErrInferM (UnknownConstructor c)
                              topLoc
          Just enumName
            -> return TypeInfo
               { ttype = TEnum enumName
               , torig = CasePattern enumName
               , tloc = ploc
               }

tcLLit :: LLit as ac c -> InferM as ac c (TypeInfo as ac c)
tcLLit (Located loc lit) = do
  enumConstrs <- constrToEnum <$> ask
  case tcLit enumConstrs lit of
    Left err  -> throwErrInferM err loc
    Right typ -> pure $
      TypeInfo typ (InferredFromLit lit) loc

tcLit :: Map EnumConstr Name -> Lit as ac c -> Either (TypeErrInfo as ac c) Type
tcLit enumConstrs lit =
  case lit of
    LInt _         -> Right TInt
    LFloat _       -> Right TFloat
    LFixed fixedn  -> Right $ tcFixedN fixedn
    LBool _        -> Right TBool
    LVoid          -> Right TVoid
    LText _        -> Right TText
    LSig _         -> Right TSig
    LAccount addr  -> Right TAccount
    LAsset addr    -> Right TAssetAny
    LContract addr -> Right TContract
    LState label   -> Right TState
    LDateTime _    -> Right TDateTime
    LTimeDelta _   -> Right TTimeDelta
    LConstr c      ->
      case Map.lookup c enumConstrs of
        Nothing -> Left (Impossible "Reference to unknown enum constructor")
        Just enum -> Right (TEnum enum)

tcFixedN :: FixedN -> Type
tcFixedN = TFixed . \case
  Fixed1 _ -> Prec1
  Fixed2 _ -> Prec2
  Fixed3 _ -> Prec3
  Fixed4 _ -> Prec4
  Fixed5 _ -> Prec5
  Fixed6 _ -> Prec6

-------------------------------------------------------------------------------
  -- Type checking of prim op calls
-------------------------------------------------------------------------------

tcPrim
  :: forall as ac c. (Ord as, Ord ac, Ord c)
  => LExpr as ac c -> Loc -> PrimOp -> [LExpr as ac c] -> InferM as ac c (TypeInfo as ac c)
tcPrim le eLoc prim argExprs = do

    -- Setup some prim op agnostic contextironment
    -- 1) Lookup what the type of the arguments of the prim op should be
    -- 2) Typecheck the arg exprs supplied to the prim op call
    -- 3) Create the type infos of the arg types
    (Sig argTypes retType) <- primSig prim

    let argTypeOrig n = FunctionArg n primNm
        mkArgTypeInfo t (lexpr, n) = TypeInfo t (argTypeOrig n) $ located lexpr
        argTypeInfos = zipWith mkArgTypeInfo argTypes (zip argExprs [1..])
        retTypeInfo = TypeInfo retType (FunctionRet primNm) eLoc

    correctArity <- arityCheck (Located eLoc primNm) argTypes argExprs
    when correctArity $ do
      argExprTypeInfos <- mapM tcLExpr argExprs

      -- Ok now typecheck
      case prim of

        -- AssetPrimOps must be typechecked uniquely-- Sometimes the type of one of
        -- the arguments to the prim op is dependent on the type of the asset
        -- supplied, and other times the _return type_ is dependent on the type of
        -- the asset supplied  as an argument.
        AssetPrimOp assetPrimOp ->
          case assetPrimOp of

            HolderBalance    -> do
              let [assetVarExpr, accExpr]  = argExprs
                  [tassetVarInfo,taccInfo] = argTypeInfos
                  [tassetInfo,taccInfo']   = argExprTypeInfos
              -- add constraint for 1st arg to be an asset type
              addConstr assetVarExpr tassetVarInfo tassetInfo
              -- add constraint for 2nd arg to be an account type
              addConstr accExpr taccInfo taccInfo'

            TransferHoldings -> do
              let [accExpr, assetVarExpr, varExpr, acc2Expr]      = argExprs
                  [tacc1Info, tassetVarInfo, tvarInfo, tacc2Info] = argTypeInfos
                  [tacc1Info', tassetInfo, tbalInfo, tacc2Info']  = argExprTypeInfos
              -- add constraint for 1st arg to be an account
              addConstr accExpr tacc1Info tacc1Info'
              -- add constraint for 2nd arg to be an asset
              addConstr assetVarExpr tassetVarInfo tassetInfo
              -- add constraint for 3rd arg depending on asset type
              addConstr varExpr tvarInfo tbalInfo
              -- add constraint for 4th arg to be an account
              addConstr acc2Expr tacc2Info tacc2Info'

            TransferTo       -> do
              let [assetVarExpr, varExpr]  = argExprs
                  [tassetVarInfo, tvarInfo] = argTypeInfos
                  [tassetInfo, tbalInfo]    = argExprTypeInfos
              -- add constraint for 1st arg to be an asset
              addConstr assetVarExpr tassetVarInfo tassetInfo
              -- add constraint for 2nd arg depending on asset type
              addConstr varExpr tvarInfo tbalInfo

            TransferFrom     -> do
              let [assetVarExpr, varExpr, accExpr]    = argExprs
                  [tassetVarInfo, tvarInfo, taccInfo] = argTypeInfos
                  [tassetInfo, tbalInfo, taccInfo']   = argExprTypeInfos
              -- add constraint for 1st arg to be an asset
              addConstr assetVarExpr tassetVarInfo tassetInfo
              -- add constraint for 2nd arg depending on asset type
              addConstr varExpr tvarInfo tbalInfo
              -- add constraint for 3rd arg to be an account
              addConstr accExpr taccInfo taccInfo'

            CirculateSupply -> do
              let [assetVarExpr, varExpr]  = argExprs
                  [tassetVarInfo, tvarInfo] = argTypeInfos
                  [tassetInfo, tbalInfo]    = argExprTypeInfos
              -- add constraint for 1st arg to be an asset
              addConstr assetVarExpr tassetVarInfo tassetInfo
              -- add constraint for 2nd arg depending on asset type
              addConstr varExpr tvarInfo tbalInfo

        CollPrimOp collPrimOp -> do

          -- However, we do some ad-hoc typechecking depending on the collection
          -- prim-op and which collection it's operating over.
          case collPrimOp of
            Aggregate -> do
              let [_, accumExpr, _]  = argExprs
                  [_, tinfoAccum, _] = argTypeInfos
                  [_, _, tinfoColl'] = argExprTypeInfos
              case tinfoAccum of
                -- The second arg must be a function, and we need to generate
                -- constraints using its arguments so we must pattern match on it.
                TypeInfo (TFun [_, arg2Type] _) torigFunc tLocFunc -> do
                  -- Add a constraint such that the 2nd argument of the aggregate
                  -- function matches the type of the values in the collection
                  addHofArgConstr accumExpr (torigFunc,tLocFunc) tinfoColl' arg2Type
                -- If the type is not a collection type, don't do anything as it
                -- will fail with a unification error later phase.
                TypeInfo t torig tloc -> pure ()

            Transform -> do
              let [funcExpr, _]   = argExprs
                  [tinfoFunc, _]  = argTypeInfos
                  [tinfoFunc', tinfoColl'] = argExprTypeInfos
              case tinfoFunc of
                -- The first arg must be a function, and we need to generate
                -- constraints using its arguments so we must pattern match on it.
                TypeInfo (TFun [argType] _) torigFunc tLocFunc -> do
                  -- Add a constraint such that the only argument of the
                  -- function passed to transform matches the type of the
                  -- values in the collection.
                  addHofArgConstr funcExpr (torigFunc, tLocFunc) tinfoColl' argType
                  case tinfoFunc' of
                    TypeInfo (TFun [_] retType) torigFunc' tLocFunc' -> do
                      -- Add a constraint that the type of the values in the
                      -- returned collection match the return type of the hof
                      -- given as an argument to 'transform'.
                      addRetTypeConstr funcExpr (torigFunc', tLocFunc') tinfoColl' retType retTypeInfo
                    -- If the type is not a function type, don't do anything as it
                    -- will fail with a unification error later phase.
                    TypeInfo t torig tloc -> pure ()
                -- If the type is not a function type, don't do anything as it
                -- will fail with a unification error later phase.
                TypeInfo t torig tloc -> pure ()

            Filter -> do
              let [funcExpr, _]   = argExprs
                  [tinfoFunc, _]  = argTypeInfos
                  [tinfoFunc', tinfoColl'] = argExprTypeInfos
              case tinfoFunc of
                -- The first arg must be a function, and we need to generate
                -- a constraint using its arguments so we must pattern match on it.
                TypeInfo (TFun [argType] _) torigFunc tLocFunc -> do
                  -- Add a constraint for the type of argument of the
                  -- function to match the type of values in the collection
                  addHofArgConstr funcExpr (torigFunc, tLocFunc) tinfoColl' argType
                -- If the type is not a function type, don't do anything as it
                -- will fail with a unification error later phase.
                TypeInfo t torig tloc -> pure ()

            Element -> do
              let [valExpr, _]  = argExprs
                  [tinfoVal, _] = argTypeInfos
                  [_, tinfoColl'] = argExprTypeInfos
              case tinfoColl' of
                TypeInfo (TColl tcoll) torigCol tlocCol -> do
                  let expectedValType =
                        case tcoll of
                          TMap _ vType -> vType
                          TSet vType   -> vType
                      tinfoValExpected = TypeInfo expectedValType (InferredFromCollType primNm tcoll) tlocCol
                  -- Add a constraint for the type of value in question to match the type of values
                  -- in the collection
                  addConstr valExpr tinfoValExpected tinfoVal
                -- If the type is not a function type, don't do anything as it
                -- will fail with a unification error later phase.
                TypeInfo t torig tloc -> pure ()

            -- There is no special typechecking to do here
            IsEmpty -> pure ()

          -- After the ad-hoc constraints are generated, generate constraints for
          -- the type sig for the primops and it's arguments.
          zipWith3M_ addConstr argExprs argTypeInfos argExprTypeInfos

        -- All other primops are typechecked simply-- The expressions supplied as
        -- arguments must unify with the types of the arguments denoted in the prim
        -- op signature.
        normalPrimOp -> zipWith3M_ addConstr argExprs argTypeInfos argExprTypeInfos

      -- The return type of all prim ops has either been contrained in the above
      -- code, or is a monomorphic type that is what it is, and should be the
      -- return type of the prim op as dictated by `primSig`
    return retTypeInfo

  where
    primNm = primName prim

    -----------------------------------------------------
    -- Helpers for Constraint Gen for Collection PrimOps
    -----------------------------------------------------

    -- The first, second, and first argument of the HO functions aggregate,
    -- transform, and filter must match the type of the values in the collection.
    addHofArgConstr :: LExpr as ac c -> (TypeOrigin as ac c, Loc) -> TypeInfo as ac c -> Type -> InferM as ac c ()
    addHofArgConstr hofExpr (hofTypeOrig, hofTypeLoc) tinfoColl argType =
      case tinfoColl of
        TypeInfo (TColl tcoll) _ tlocCol -> do
          let tinfoCollVals vType = TypeInfo vType (InferredFromCollType primNm tcoll) tlocCol
          -- Depending on the collection type, diff constraints are generated...
          case tcoll of
            -- For maps, the value type must unify with the second function arg type.
            TMap _ vType -> addConstr hofExpr (tinfoCollVals vType) tinfoArg
            TSet vType   -> addConstr hofExpr (tinfoCollVals vType) tinfoArg
        -- If the type is not a collection type, don't do anything as it
        -- will fail with a unification error later phase.
        TypeInfo t torig tloc -> pure ()
      where
        tinfoArg = TypeInfo argType hofTypeOrig hofTypeLoc

    -- Add a constraint on the return type of the prim op to be the new type of
    -- the collection after applying the 'transform' primop.
    addRetTypeConstr :: LExpr as ac c -> (TypeOrigin as ac c, Loc) -> TypeInfo as ac c -> Type -> TypeInfo as ac c -> InferM as ac c ()
    addRetTypeConstr hofExpr (torigHof, tlocHof) tinfoColl newValType retTypeInfo =
      case tinfoColl of
        TypeInfo (TColl tcoll) torigCol tlocCol -> do
          let newCollType =
                case tcoll of
                  TMap kType _ -> TMap kType newValType
                  TSet _       -> TSet newValType
          -- Unify the return type of the whole primop
          -- expression with the map type resulting from the
          -- transformation function.
          let retCollTypeInfo = TypeInfo (TColl newCollType) torigHof tlocHof
          addConstr hofExpr retTypeInfo retCollTypeInfo
        -- If the type is not an collection type, don't do anything as
        -- it will fail with a unification error.
        TypeInfo t torig tloc -> pure ()

-- | Type signatures of builtin primitive operations.
primSig :: PrimOp -> InferM as ac c Sig
primSig = \case
  Verify              -> pure $ Sig [TAccount, TText, TSig] TBool
  Sign                -> pure $ Sig [TText] TSig
  Block               -> pure $ Sig [] TInt
  Deployer            -> pure $ Sig [] TAccount
  Sender              -> pure $ Sig [] TAccount
  Created             -> pure $ Sig [] TDateTime
  Address             -> pure $ Sig [] TContract
  Validator           -> pure $ Sig [] TAccount
  Sha256              -> pure $ Sig [TAny] TText
  AccountExists       -> pure $ Sig [TAccount] TBool
  AssetExists         -> freshTAVar >>= \tav -> pure (Sig [tav] TBool)
  ContractExists      -> pure $ Sig [TContract] TBool
  Terminate           -> pure $ Sig [] TTransition
  Now                 -> pure $ Sig [] TDateTime
  TransitionTo        -> pure $ Sig [TState] TTransition
  Stay                -> pure $ Sig [] TTransition
  CurrentState        -> pure $ Sig [] TState
  TxHash              -> pure $ Sig [] TText
  ContractValue       -> Sig [TContract, TText]   <$> freshTVar
  ContractValueExists -> pure $ Sig [TContract, TText] TBool
  ContractState       -> pure $ Sig [TContract] TState
  IsBusinessDayUK     -> pure $ Sig [TDateTime] TBool
  NextBusinessDayUK   -> pure $ Sig [TDateTime] TDateTime
  IsBusinessDayNYSE   -> pure $ Sig [TDateTime] TBool
  NextBusinessDayNYSE -> pure $ Sig [TDateTime] TDateTime
  Between             -> pure $ Sig [TDateTime, TDateTime, TDateTime] TDateTime
  TimeDiff            -> pure $ Sig [TDateTime, TDateTime] TTimeDelta
  Fixed1ToFloat       -> pure $ Sig [TFixed Prec1] TFloat
  Fixed2ToFloat       -> pure $ Sig [TFixed Prec2] TFloat
  Fixed3ToFloat       -> pure $ Sig [TFixed Prec3] TFloat
  Fixed4ToFloat       -> pure $ Sig [TFixed Prec4] TFloat
  Fixed5ToFloat       -> pure $ Sig [TFixed Prec5] TFloat
  Fixed6ToFloat       -> pure $ Sig [TFixed Prec6] TFloat
  FloatToFixed1       -> pure $ Sig [TFloat] (TFixed Prec1)
  FloatToFixed2       -> pure $ Sig [TFloat] (TFixed Prec2)
  FloatToFixed3       -> pure $ Sig [TFloat] (TFixed Prec3)
  FloatToFixed4       -> pure $ Sig [TFloat] (TFixed Prec4)
  FloatToFixed5       -> pure $ Sig [TFloat] (TFixed Prec5)
  FloatToFixed6       -> pure $ Sig [TFloat] (TFixed Prec6)
  AssetPrimOp a       -> assetPrimSig a
  MapPrimOp m         -> mapPrimSig m
  SetPrimOp m         -> setPrimSig m
  CollPrimOp c        -> collPrimSig c

assetPrimSig :: AssetPrimOp -> InferM as ac c Sig
assetPrimSig apo = do
  (assetVar, holdingsVar) <-
    freshTAVar >>= \tav ->
      pure (tav, TVar (THV tav))
  case apo of
    HolderBalance    -> pure $ Sig [assetVar, TAccount] holdingsVar
    TransferHoldings -> pure $ Sig [TAccount, assetVar, holdingsVar, TAccount] TVoid -- from Account to Account
    TransferTo       -> pure $ Sig [assetVar, holdingsVar] TVoid                    -- from Account to Contract
    TransferFrom     -> pure $ Sig [assetVar, holdingsVar, TAccount] TVoid          -- from Contract to Account
    CirculateSupply  -> pure $ Sig [assetVar, holdingsVar] TVoid                    -- from Asset Supply to Asset issuer's holdings

mapPrimSig :: MapPrimOp -> InferM as ac c Sig
mapPrimSig = \case
  MapInsert -> do
    a <- freshTVar
    b <- freshTVar
    pure $ Sig [a, b, TColl (TMap a b)] (TColl (TMap a b))
  MapDelete -> do
    a <- freshTVar
    b <- freshTVar
    pure $ Sig [a, TColl (TMap a b)] (TColl (TMap a b))
  MapLookup -> do
    a <- freshTVar
    b <- freshTVar
    pure $ Sig [a, TColl (TMap a b)] b
  MapModify -> do
    a <- freshTVar
    b <- freshTVar
    pure $ Sig [a, TFun [b] b, TColl (TMap a b)] (TColl (TMap a b))

setPrimSig :: SetPrimOp -> InferM as ac c Sig
setPrimSig = \case
  SetInsert -> do
    a <- freshTVar
    pure $ Sig [a, TColl (TSet a)] (TColl (TSet a))
  SetDelete -> do
    a <- freshTVar
    pure $ Sig [a, TColl (TSet a)] (TColl (TSet a))

collPrimSig :: CollPrimOp -> InferM as ac c Sig
collPrimSig = \case
  Aggregate -> do
    a <- freshTVar
    b <- freshTVar
    c <- freshTCVar
    pure $ Sig [a, TFun [a,b] a, c] a
  Transform -> do
    a <- freshTVar
    b <- freshTVar
    c <- freshTCVar
    d <- freshTCVar
    pure $ Sig [TFun [a] b, c] d
  Filter -> do
    a <- freshTVar
    b <- freshTCVar
    c <- freshTCVar
    pure $ Sig [TFun [a] TBool, b] c
  Element -> do
    a <- freshTVar
    b <- freshTCVar
    pure $ Sig [a, b] TBool
  IsEmpty -> do
    a <- freshTCVar
    pure $ Sig [a] TBool

-------------------------------------------------------------------------------
-- Valid Binary Op logic
-------------------------------------------------------------------------------

tcUnOp :: (Ord as, Ord ac, Ord c) => LUnOp -> LExpr as ac c -> InferM as ac c (TypeInfo as ac c)
tcUnOp (Located opLoc op) e = do
    tcUnOp' opLoc (UnaryOperator op) e
  where
    tcUnOp' =
      case op of
        Not -> tcNotOp

tcBinOp :: (Ord as, Ord ac, Ord c)
  => LBinOp -> LExpr as ac c -> LExpr as ac c -> InferM as ac c (TypeInfo as ac c)
tcBinOp (Located opLoc op) e1 e2 = do
    tcBinOp' opLoc (BinaryOperator op) e1 e2
  where
    tcBinOp' =
      case op of
        Mul -> tcMult
        Add -> tcAddSub op
        Sub -> tcAddSub op
        Div -> tcDiv
        And -> tcAndOr
        Or  -> tcAndOr
        Equal   -> tcEqual Equal
        LEqual  -> tcLEqual LEqual
        GEqual  -> tcGEqual
        Greater -> tcGreater
        Lesser  -> tcLesser
        NEqual  -> tcNEqual

-- | Multiplication is only valid for:
--     TDelta * TInt
--     TInt * TDelta
--     TInt   * TInt
--     TFloat * TFloat
--     TFixed * TFixed
tcMult
  :: (Ord as, Ord ac, Ord c)
  => Loc -> TypeOrigin as ac c -> LExpr as ac c -> LExpr as ac c -> InferM as ac c (TypeInfo as ac c)
tcMult opLoc torig e1 e2 = do
  tinfo1 <- tcLExpr e1
  tinfo2 <- tcLExpr e2
  case (ttype tinfo1, ttype tinfo2) of

    -- The reason for these alternating pattern matches is to correctly
    -- typecheck unusual binops in which both expression the operation is over
    -- do not need to necessarily have the same type, albeit a specific one.
    (TTimeDelta, _)   -> addConstrAndRetInfo' (tDeltaInfo torig eLoc) (tIntInfo torig opLoc, tinfo2)
    (_, TTimeDelta)   -> addConstrAndRetInfo' (tDeltaInfo torig eLoc) (tinfo1, tIntInfo torig opLoc)

    (TInt, _)         -> addConstrAndRetInfo' (tIntInfo torig eLoc) (tIntInfo torig opLoc, tinfo2)
    (TFloat, _)       -> addConstrAndRetInfo' (tFloatInfo torig eLoc) (tFloatInfo torig opLoc, tinfo2)
    (TFixed p, _)     -> addConstrAndRetInfo' (tFixedInfo p torig eLoc) (tFixedInfo p torig opLoc, tinfo2)
    (TVar a, _)       -> addConstrAndRetInfo' tinfo1 (tinfo1, tinfo2)
    (t1,t2)           -> do
      throwErrInferM (InvalidBinOp Mul t1 t2) opLoc
      return $ TypeInfo TError torig eLoc
  where
    eLoc = located e1
    addConstrAndRetInfo' = addConstrAndRetInfo e2

-- | Add, Sub is only valid for:
--     TInt           +/- TInt
--     TFloat         +/- Float
--     TFixed         +/- TFixed
--     TDatetime      +/- TDelta
--     TDelta          +  TDelta
--     TText            +  TText (concatenation)
tcAddSub
  :: (Ord as, Ord ac, Ord c)
  => BinOp -> Loc -> TypeOrigin as ac c -> LExpr as ac c -> LExpr as ac c -> InferM as ac c (TypeInfo as ac c)
tcAddSub op opLoc torig e1 e2 = do
  tinfo1 <- tcLExpr e1
  tinfo2 <- tcLExpr e2
  case (ttype tinfo1, ttype tinfo2) of

    -- Constrain the LHS to be the only type expected in the binary operation
    -- that it can be, due to binary op definitions in FCL.
    (TInt, _)         -> addConstrAndRetInfo' (tIntInfo torig eLoc) (tIntInfo torig opLoc, tinfo2)
    (TFloat, _)       -> addConstrAndRetInfo' (tFloatInfo torig eLoc) (tFloatInfo torig opLoc, tinfo2)
    (TFixed p, _)     -> addConstrAndRetInfo' (tFixedInfo p torig eLoc) (tFixedInfo p torig opLoc, tinfo2)
    (TDateTime, _)    -> addConstrAndRetInfo' (tDatetimeInfo torig eLoc) (tDeltaInfo torig opLoc, tinfo2)
    -- TDelta + TDelta (no subtraction)
    (TTimeDelta, _)   -> tcAddNoSub tinfo1 tinfo2
    -- TText + TText (concatenation, no subtraction)
    (TText, _)         -> tcAddNoSub tinfo1 tinfo2
    (TVar a, _)       -> addConstrAndRetInfo' tinfo1 (tinfo1, tinfo2)
    (t1, t2)          -> do
      throwErrInferM (InvalidBinOp op t1 t2) opLoc
      return $ TypeInfo TError torig eLoc
  where
    eLoc = located e1
    addConstrAndRetInfo' = addConstrAndRetInfo e2

    tcAddNoSub tinfo1 tinfo2
      | op == Sub = do
          throwErrInferM (InvalidBinOp op constructor typ) opLoc
          return $ TypeInfo TError torig eLoc
      | otherwise = addConstrAndRetInfo' (tinfoMaker torig eLoc) (tinfo1, tinfoMaker torig opLoc)
      where
        (typ, (tinfoMaker, constructor)) =
          (ttype tinfo2,) $ case ttype tinfo1 of
            TText       -> (tMsgInfo, TText)
            TTimeDelta -> (tDeltaInfo, TTimeDelta)
            otherwise  -> panic "The only invalid addition-no-subtraction types are TText and TTimeDelta"

-- | Division is only valid for:
--     TInt   / TInt
--     TFloat / TFLoat
--     TFixed / TFixed
tcDiv
  :: (Ord as, Ord ac, Ord c)
  => Loc -> TypeOrigin as ac c -> LExpr as ac c -> LExpr as ac c -> InferM as ac c (TypeInfo as ac c)
tcDiv opLoc torig e1 e2 = do
  tinfo1 <- tcLExpr e1
  tinfo2 <- tcLExpr e2
  case (ttype tinfo1, ttype tinfo2) of
    (TInt, _)         -> addConstrAndRetInfo' (tIntInfo torig eLoc) (tIntInfo torig opLoc, tinfo2)
    (TFloat, _)       -> addConstrAndRetInfo' (tFloatInfo torig eLoc) (tFloatInfo torig opLoc, tinfo2)
    (TFixed p, _)     -> addConstrAndRetInfo' (tFixedInfo p torig eLoc) (tFixedInfo p torig opLoc, tinfo2)
    (TVar a, _)       -> addConstrAndRetInfo' tinfo1 (tinfo1, tinfo2)
    (t1,t2)           -> do
      throwErrInferM (InvalidBinOp Div t1 t2) opLoc
      return $ TypeInfo TError torig eLoc
  where
    eLoc = located e1
    addConstrAndRetInfo' = addConstrAndRetInfo e2

tcAndOr
  :: (Ord as, Ord ac, Ord c)
  => Loc -> TypeOrigin as ac c -> LExpr as ac c -> LExpr as ac c -> InferM as ac c (TypeInfo as ac c)
tcAndOr opLoc torig e1 e2 = do
  tinfo1 <- tcLExpr e1
  tinfo2 <- tcLExpr e2
  let argTypeInfo = TypeInfo TBool torig opLoc
  addConstr e2 argTypeInfo tinfo1
  addConstr e2 argTypeInfo tinfo2
  return $ TypeInfo TBool torig (located e2)

tcEqual
  :: (Ord as, Ord ac, Ord c)
  => BinOp -> Loc -> TypeOrigin as ac c -> LExpr as ac c -> LExpr as ac c -> InferM as ac c (TypeInfo as ac c)
tcEqual op opLoc torig e1 e2 = do
  tinfo1 <- tcLExpr e1
  tinfo2 <- tcLExpr e2
  case (ttype tinfo1, ttype tinfo2) of
    (TContract, _) -> addConstrAndRetBool (tContractInfo torig opLoc, tinfo2)
    (TInt, _)      -> addConstrAndRetBool (tIntInfo torig opLoc, tinfo2)
    (TFloat, _)    -> addConstrAndRetBool (tFloatInfo torig opLoc, tinfo2)
    (TFixed p, _)  -> addConstrAndRetBool (tFixedInfo p torig opLoc, tinfo2)
    (TAccount, _)  -> addConstrAndRetBool (tAccountInfo torig opLoc, tinfo2)
    (TBool, _)     -> addConstrAndRetBool (tBoolInfo torig opLoc, tinfo2)
    (TAsset at, _) -> addConstrAndRetBool (tAssetInfo at torig opLoc, tinfo2)
    (TDateTime, _) -> addConstrAndRetBool (tDatetimeInfo torig opLoc, tinfo2)
    (TTimeDelta, _) -> addConstrAndRetBool (tDeltaInfo torig opLoc, tinfo2)
    (TText, _)      -> addConstrAndRetBool (tMsgInfo torig opLoc, tinfo2)
    (TVar a, _)    -> addConstrAndRetBool (tinfo1, tinfo2)
    (t1,t2)        -> do
      throwErrInferM (InvalidBinOp op t1 t2) opLoc
      return $ tBoolInfo torig eLoc
  where
    eLoc = located e2
    addConstrAndRetBool = addConstrAndRetInfo e2 $ tBoolInfo torig eLoc

tcLEqual
  :: (Ord as, Ord ac, Ord c)
  => BinOp -> Loc -> TypeOrigin as ac c -> LExpr as ac c -> LExpr as ac c -> InferM as ac c (TypeInfo as ac c)
tcLEqual op opLoc torig e1 e2 = do
  tinfo1 <- tcLExpr e1
  tinfo2 <- tcLExpr e2
  case (ttype tinfo1, ttype tinfo2) of
    (TInt, _)        -> addConstrAndRetBool (tIntInfo torig opLoc, tinfo2)
    (TFloat, _)      -> addConstrAndRetBool (tFloatInfo torig opLoc, tinfo2)
    (TFixed p, _)    -> addConstrAndRetBool (tFixedInfo p torig opLoc, tinfo2)
    (TDateTime, _)   -> addConstrAndRetBool (tDatetimeInfo torig opLoc, tinfo2)
    (TTimeDelta, _)  -> addConstrAndRetBool (tDeltaInfo torig opLoc, tinfo2)
    (TVar a, _)      -> addConstrAndRetBool (tinfo1, tinfo2)
    (t1,t2)          -> do
      throwErrInferM (InvalidBinOp op t1 t2) opLoc
      return $ TypeInfo TError torig eLoc
  where
    eLoc = located e2
    addConstrAndRetBool = addConstrAndRetInfo e2 $ tBoolInfo torig eLoc

tcGEqual :: (Ord as, Ord ac, Ord c) => Loc -> TypeOrigin as ac c -> LExpr as ac c -> LExpr as ac c -> InferM as ac c (TypeInfo as ac c)
tcGEqual = tcLEqual GEqual

tcGreater :: (Ord as, Ord ac, Ord c) => Loc -> TypeOrigin as ac c -> LExpr as ac c -> LExpr as ac c -> InferM as ac c (TypeInfo as ac c)
tcGreater = tcLEqual Greater

tcLesser :: (Ord as, Ord ac, Ord c) => Loc -> TypeOrigin as ac c -> LExpr as ac c -> LExpr as ac c -> InferM as ac c (TypeInfo as ac c)
tcLesser = tcLEqual Lesser

tcNEqual :: (Ord as, Ord ac, Ord c) => Loc -> TypeOrigin as ac c -> LExpr as ac c -> LExpr as ac c -> InferM as ac c (TypeInfo as ac c)
tcNEqual = tcEqual NEqual

tcNotOp :: (Ord as, Ord ac, Ord c) => Loc -> TypeOrigin as ac c -> LExpr as ac c -> InferM as ac c (TypeInfo as ac c)
tcNotOp opLoc torig e1 = do
  tinfo <- tcLExpr e1
  case ttype tinfo of
    TBool       -> return $ tBoolInfo torig eLoc
    invalidUnOp -> do
      throwErrInferM (InvalidUnOp Not invalidUnOp) opLoc
      return $ TypeInfo TError torig eLoc
  where
    eLoc = located e1

-- | Helper for common pattern "Add constraint of two TypeInfos and return a TypeInfo"
addConstrAndRetInfo :: LExpr as ac c -> TypeInfo as ac c -> (TypeInfo as ac c, TypeInfo as ac c) -> InferM as ac c (TypeInfo as ac c)
addConstrAndRetInfo le retInfo (expected, actual) =
  addConstr le expected actual >> return retInfo


--------------------------------------------------------------------------------
-- Checking method preconditions
--------------------------------------------------------------------------------

tcPreconditions :: forall as ac c. (Ord as, Ord ac, Ord c) => Preconditions as ac c -> InferM as ac c ()
tcPreconditions (Preconditions ps) = mapM_ (tcPrecondition) ps
  where
    tcPrecondition :: (Precondition, LExpr as ac c) -> InferM as ac c ()
    tcPrecondition (p,e) = addConstr e tyExpected =<< tcLExpr e
      where
        tyExpected = TypeInfo
            { ttype = ty
            , torig = FromPrecondition p
            , tloc  = located e
            }
        ty = case p of
            PrecAfter  -> TDateTime
            PrecBefore -> TDateTime
            PrecRoles  -> case locVal e of
              ESet _ -> TColl (TSet TAccount)
              _      -> TAccount


-------------------------------------------------------------------------------
-- Contraint Generation
-------------------------------------------------------------------------------

data Constraint as ac c = Constraint
  { mOrigLExpr :: Maybe (LExpr as ac c) -- ^ Maybe the expression from which the constraint originated
  , expected :: TypeInfo as ac c
  , actual   :: TypeInfo as ac c
  } deriving (Show)

-- | Add a constraint during the constraint generation phase
addConstr :: LExpr as ac c -> TypeInfo as ac c -> TypeInfo as ac c -> InferM as ac c ()
addConstr lexpr expected' actual' = modify' $ \s ->
  s { constraints = constraints s ++ [c] }
  where
    c = Constraint
      { mOrigLExpr = Just lexpr
      , expected = expected'
      , actual   = actual'
      }

instance (Eq as, Eq ac, Eq c, Pretty as, Pretty ac, Pretty c) => Pretty (Constraint as ac c) where
  ppr (Constraint mLExpr ti1 ti2) =
    "Constraint:"
    <$$+> (maybe "" (ppr . locVal) mLExpr)
    <$$+> ppr ti1
    <$$+> ppr ti2

instance (Eq as, Eq ac, Eq c, Pretty as, Pretty ac, Pretty c) => Pretty [Constraint as ac c] where
  ppr [] = ""
  ppr (c:cs) = ppr c <$$> Script.Pretty.line <> ppr cs

-------------------------------------------------------------------------------
-- Inference Utils
-------------------------------------------------------------------------------

typeVars :: [Text]
typeVars = [1..] >>= flip replicateM ['a'..'z'] >>= return . toS

freshText :: InferM as ac c Text
freshText = do
  inferState <- get
  put $ inferState { count = count inferState + 1 }
  return $ (typeVars `unsafeIndex` count inferState)

freshTVar :: InferM as ac c Type
freshTVar = TVar <$> freshTVar'

freshTVar' :: InferM as ac c TVar
freshTVar' = TV <$> freshText

freshTAVar :: InferM as ac c Type
freshTAVar = TVar <$> freshTAVar'

freshTAVar' :: InferM as ac c TVar
freshTAVar' = TAV <$> freshText

freshTCVar :: InferM as ac c Type
freshTCVar = TVar <$> freshTCVar'

freshTCVar' :: InferM as ac c TVar
freshTCVar' = TCV <$> freshText

lookupVarType :: LName -> InferM as ac c (Maybe (TMeta, TypeInfo as ac c))
lookupVarType (Located loc name) = lookupContext name <$> gets context

-- | Just like 'lookupVarType' but throws a type error if the variable doesn't
-- exist in the typing env.
lookupVarType' :: LName -> InferM as ac c (TMeta, TypeInfo as ac c)
lookupVarType' var@(Located loc name) = do
  mVarTypeInfo <- lookupVarType var
  case mVarTypeInfo of
    Nothing -> (Temp,) <$> throwErrInferM (UnboundVariable name) loc
    Just typeInfo -> return typeInfo

-- | Checks if # args supplied to function match # args in Sig,
-- returns a boolean indicating whether this is true or not.
arityCheck :: LName -> [Type] -> [LExpr as ac c] -> InferM as ac c Bool
arityCheck (Located loc nm) typs args
  | lenTyps == lenArgs = pure True
  | otherwise = do
  throwErrInferM (ArityFail nm lenTyps lenArgs) loc
  pure False
  where
    lenTyps = length typs
    lenArgs = length args

-------------------------------------------------------------------------------
-- Substitution
-------------------------------------------------------------------------------

class Substitutable a where
  apply :: Subst -> a -> a

instance Substitutable Type where
  apply s t@(TVar a)  =
    case a of
      THV t' -> TVar $ THV $ apply s t'
      _ -> Map.findWithDefault t a (unSubst s)
  apply s TError      = TError
  apply s TInt        = TInt
  apply s TFloat      = TFloat
  apply s (TFixed p)  = TFixed p
  apply s TBool       = TBool
  apply s TAccount    = TAccount
  apply s (TAsset at) = TAsset at
  apply s TContract   = TContract
  apply s TText       = TText
  apply s TVoid       = TVoid
  apply s TSig        = TSig
  apply s TAny        = TAny
  apply s TAssetAny   = TAssetAny
  apply s TState      = TState
  apply s TDateTime   = TDateTime
  apply s (TFun ats rt) = TFun (map (apply s) ats) (apply s rt)
  apply s t@TEnum{}   = t
  apply s TTimeDelta  = TTimeDelta
  apply s (TColl tc)  = TColl (apply s tc)
  apply s TTransition = TTransition

instance Substitutable TCollection where
  apply s (TMap k v) = TMap (apply s k) (apply s v)
  apply s (TSet v)   = TSet (apply s v)

instance (Substitutable a, Substitutable b) => Substitutable (a,b) where
  apply s (a,b) = (apply s a, apply s b)

instance Substitutable (TypeInfo as ac c) where
  apply s (TypeInfo t info tPos) = TypeInfo (apply s t) info tPos

instance Substitutable TMeta where
  apply s tmeta = tmeta

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply

instance Substitutable (Constraint as ac c) where
  apply s (Constraint e t1 t2) = Constraint e (apply s t1) (apply s t2)

newtype Subst = Subst { unSubst :: Map.Map TVar Type }

instance Semigroup Subst where
  (<>) = composeSubst

instance Monoid Subst where
  mempty = Subst mempty

instance Pretty Subst where
  ppr (Subst s) =
    case Map.toList s of
      [] -> ""
      ((k,v):m') -> ppr k <+> "=>" <+> ppr v
               <$$> ppr (Subst (Map.fromList m'))

composeSubst :: Subst -> Subst -> Subst
composeSubst s@(Subst s1) (Subst s2) =
  Subst (Map.map (apply s) s2 `Map.union` s1)

-------------------------------------------------------------------------------
-- Unification & Solving
-------------------------------------------------------------------------------

data Unifier as ac c = Unifier Subst [Constraint as ac c]

instance Semigroup (Unifier as ac c) where
  (Unifier subst cs) <> (Unifier subst' cs') =
    Unifier (subst' `composeSubst` subst) (cs' ++ apply subst' cs)

instance Monoid (Unifier as ac c) where
  mempty = Unifier mempty mempty

instance {-# OVERLAPS #-} Semigroup (Either a (Unifier as ac c)) where
  -- Note: Short circuits on Left, does not accumulate values.
  (Left e) <> _           = Left e
  _ <> (Left e)          = Left e
  (Right u) <> (Right u') = Right (u <> u')

instance {-# OVERLAPS #-} Monoid (Either a (Unifier as ac c)) where
  mempty = Right mempty

instance (Eq as, Eq ac, Eq c, Pretty as, Pretty ac, Pretty c) => Pretty (Unifier as ac c) where
  ppr (Unifier subst cs) = "Unifier:"
               <$$+> ("Subst:" <$$+> ppr subst)
               <$$+> ("Constraints:" <$$+> ppr cs)

type SolverM as ac c = State [TypeError as ac c]

runSolverM :: forall as ac c. InferState as ac c -> Either (NonEmpty (TypeError as ac c)) Subst
runSolverM inferState = case holeErrs <> typeErrs of
    [] -> Right (Subst subst)
    e:es -> Left (e :| es)
  where
    -- type variable substitutions and any type errors
    (Subst subst, typeErrs) = runState
        (solver $ Unifier mempty (constraints inferState))
        (errs inferState)
    -- turn any holes into a type error
    holeErrs = map mkHoleError . toList . holes $ inferState
    -- look up the inferred type
    mkHoleError :: Hole as ac c -> TypeError as ac c
    mkHoleError Hole{ holeLocation, holeContext, holeTyVar }
      = case Map.lookup holeTyVar subst of
          Nothing ->
              TypeError{ errLoc = holeLocation, errInfo = UnknownHoleInProgram }
          Just inferredType ->
              TypeError{ errLoc = holeLocation, errInfo = holeInfo }
            where
              holeInfo = HoleInProgram{ inferredType, suggestions }
              suggestions
                = filter ((==) inferredType . ttype . snd . snd)
                . Map.toList
                . sconcat
                . unContext
                $ holeContext

throwErrSolverM :: TypeError as ac c -> SolverM as ac c (Unifier as ac c)
throwErrSolverM typeErr = do
  modify' $ flip (++) [typeErr]
  return mempty

bind ::  TVar -> Type -> Unifier as ac c
bind tv t = Unifier (Subst (Map.singleton tv t)) []

unify :: forall as ac c. Constraint as ac c -> SolverM as ac c (Unifier as ac c)
unify (Constraint mLExpr to1 to2) =
    case unify' t1 t2 of
      Left tErrInfo -> throwErrSolverM $ TypeError tErrInfo terrLoc
      Right unifier -> return unifier
  where
    TypeInfo t1 t1orig t1Pos = to1
    TypeInfo t2 t2orig t2Pos = to2

    terrLoc = maybe t2Pos located mLExpr

    unify' :: Type -> Type -> Either (TypeErrInfo as ac c) (Unifier as ac c)
    unify' t1 t2 | t1 == t2          = Right mempty
    unify' TAny   _                  = Right mempty
    unify' _      TAny               = Right mempty
    unify' (TVar v) t                = unifyTVar t v
    unify' t (TVar v)                = unifyTVar t v
    unify' TAssetAny (TAsset ta)     = Right mempty
    unify' (TAsset ta) TAssetAny     = Right mempty
    unify' TError t                  = Right mempty
    unify' t TError                  = Right mempty
    unify' (TColl tc1) (TColl tc2)   = unifyTColl tc1 tc2
    unify' (TFun as r) (TFun as' r') = unifyTFun (as,r) (as',r')
    unify' t1 t2                     = Left $ UnificationFail to1 to2

    -- Generate unifiers for all arguments/return types for both functions. Here
    -- we do not generate constraints as per usual unification of functions for
    -- a bit more explicit type error (reporting that the function types don't
    -- unify, rather than the individual arg/ret type pairs don't unify).
    unifyTFun :: ([Type], Type) -> ([Type], Type) -> Either (TypeErrInfo as ac c) (Unifier as ac c)
    unifyTFun (as,r) (as',r')
      | length as == length as' =
          foldMap (uncurry unify') (zip (as ++ [r]) (as' ++ [r']))
        -- Right (mempty, constraints)
      | otherwise = Left $ UnificationFail to1 to2 -- Fail because of arity mismatch

    unifyTColl :: TCollection -> TCollection -> Either (TypeErrInfo as ac c) (Unifier as ac c)
    unifyTColl (TMap k1 v1) (TMap k2 v2) = unify' k1 k2 >> unify' v1 v2
    unifyTColl (TSet v1)    (TSet v2)    = unify' v1 v2
    unifyTColl _            _            = Left $ UnificationFail to1 to2

    unifyTVar :: Type -> TVar -> Either (TypeErrInfo as ac c) (Unifier as ac c)
    -- TV (general type vars) unify with anything
    unifyTVar t            v@(TV _)  = Right $ v `bind` t
    unifyTVar t@(TVar (TV _)) v      = Right $ v `bind` t

   -- TAV (asset specific type vars) unify with only 3 types
    unifyTVar TInt         v@(THV (TAsset TDiscrete)) = Right $ v `bind` TInt
    unifyTVar TBool        v@(THV (TAsset TBinary)) = Right $ v `bind` TBool
    unifyTVar t@(TFixed p) v@(THV (TAsset (TFractional p')))
      | p == p' = Right $ v `bind` t
      | otherwise = Left (UnificationFail to1 to2)
    unifyTVar t@(TVar (THV _)) v@(THV _) = Right $ v `bind` t -- THVar binds with THVar
    unifyTVar _            v@(THV _) = Left $ UnificationFail to1 to2


    unifyTVar t@(TAsset ta) v@(TAV tavar) = Right $ v `bind` t
    unifyTVar t@(TVar (TAV _)) v@(TAV _) = Right $ v `bind` t -- TAVar binds with TAVar
    unifyTVar TAssetAny     v@(TAV _) = Right mempty
    unifyTVar _             v@(TAV _) = Left $ UnificationFail to1 to2

    -- TCV (collection specific type vars)
    unifyTVar t@(TColl _)      v@(TCV c) = Right $ v `bind` t
    unifyTVar t@(TVar (TCV _)) v@(TCV c) = Right $ v `bind` t -- TCVar binds with TCVar
    unifyTVar _                v@(TCV _) = Left $ UnificationFail to1 to2


solver :: Unifier as ac c -> SolverM as ac c Subst
solver (Unifier subst initConstrs) =
  case initConstrs of
    [] -> return subst
    (c:cs) -> do
      resUnifier <- unify c
      let processedUnifier = Unifier subst cs
      -- Debugging:
      -- traceM (prettyPrint $ processedUnifier <> resUnifier)
      solver (processedUnifier <> resUnifier)

-------------------------------------------------------------------------------
-- Pretty Printer
-------------------------------------------------------------------------------

instance Pretty Sig where
  ppr (Sig argtys TVoid) = tupleOf argtys <+> "->" <+> "()"
  ppr (Sig argtys retty) = tupleOf argtys <+> "->" <+> ppr retty

instance (Eq as, Eq ac, Eq c, Pretty as, Pretty ac, Pretty c) => Pretty (TypeOrigin as ac c) where
  ppr torig = case torig of
    OutOfThinAir        -> "is a fresh type variable"
    ExpectedFromSeq -> "is the expected type of the statement"
    ExpectedFromMethodBody -> "is the expected type of the method body"
    VariableDefn nm     -> "was inferred by top level definition for variable" <+> squotes (ppr nm)
    InferredFromVar nm  -> "was inferred variable" <+> squotes (ppr nm)
    InferredFromLit lit -> "was inferred from literal" <+> squotes (ppr lit)
    InferredFromExpr e  -> "was inferred from expression" <+> squotes (ppr e)
    InferredFromAssetType nm t -> "was inferred from the asset type" <+> squotes (ppr t) <+> "supplied as an argument to the primop" <+> squotes (ppr nm)
    InferredFromHelperDef nm -> "was inferred from the helper function definition" <+> squotes (ppr nm)
    InferredFromAssignment nm -> "was inferred from the assignment to variable" <+> squotes (ppr nm)
    InferredFromMethodBody -> "was inferred from the body of method"
    BinaryOperator op   -> "was inferred from use of binary operator" <+> squotes (ppr op)
    UnaryOperator op    -> "was inferred from use of unary operator" <+> squotes (ppr op)
    Assignment          -> "was inferred from variable assignment"
    IfCondition         -> "must be a bool because of if statement"
    DateTimeGuardPred   -> "must be a datetime because it is a datetime guard predicate"
    DateTimeGuardBody   -> "must be a void because it is the body of a datetime guard"
    FunctionArg n nm    -> "was inferred from the type signature of argument" <+> ppr n <+> "of function" <+> squotes (ppr nm)
    FunctionRet nm      -> "was inferred from the return type of the function" <+> squotes (ppr nm)
    CasePattern nm      -> "was inferred from type of case pattern" <+> squotes (ppr nm)
    CaseBody e          -> "was inferred from type of case body" <+> squotes (ppr e)
    InferredFromCollType nm t -> "was inferred from the collection type" <+> squotes (ppr t) <+> "supplied as an argument to the primop" <+> squotes (ppr nm)
    EmptyCollection     -> "was inferred from an empty collection"
    MapExpr             -> "was inferred from a map expression"
    SetExpr             -> "was inferred from a set expression"
    FromPrecondition p -> "was inferred from a precondition" <+> squotes (ppr p)
    FromHole            -> "coming from a hole in the program"
    EmptyBlock          -> "was inferred from an empty block (e.g. if-statement without else-branch)"

instance (Eq as, Eq ac, Eq c, Pretty as, Pretty ac, Pretty c) => Pretty (TypeInfo as ac c) where
  ppr (TypeInfo t orig loc) = "Type" <+> ppr t <+> ppr orig <+> "on" <+> ppr loc

instance (Eq as, Eq ac, Eq c, Pretty as, Pretty ac, Pretty c) => Pretty (TypeErrInfo as ac c) where
  ppr e = case e of
    UnboundVariable nm            -> "Unbound variable: " <+> ppr nm
    Shadow id meta (TypeInfo t _ loc)
      -> "The binding" <+> sqppr id <+> "shadows the" <+> ppr meta
      <$$+> "of the same name, of type" <+> sqppr t <> ","
      <$$+> "bound at" <+> ppr loc <> "."
    InvalidDefinition nm e lhsTyp rhsTyp
                                  -> "Invalid definition for" <+> ppr nm <> ":"
                                  <$$+> "Expected type:" <+> ppr lhsTyp
                                  <$$+> "But inferred type:" <+> ppr rhsTyp <+> "for expression" <+> squotes (ppr e)
    UndefinedFunction nm          -> "Invalid function name: " <+> ppr nm
    InvalidBinOp op t1 t2         -> "Invalid binary operation: "
                                  <$$+> squotes (ppr op) <+> "does not accept types" <+> ppr t1 <+> "and" <+> ppr t2
    InvalidUnOp op t              -> "Invalid unary operation: "
                                  <$$+> squotes (ppr op) <+> "does not accept types" <+> ppr t
    InvalidAddress nm             -> "Invalid address: " <+> ppr nm
                                  <$$+> "Addresses must be a valid base 58 encoded sha256 hashes."
    ArityFail nm n m              -> "Arity mismatch in function call" <+> ppr nm <> ":"
                                  <$$+> "Expecting" <+> ppr n <+> "arguments, but got" <+> ppr m
    InvalidArgType nm t1 t2       -> "Invalid argument type to method" <+> ppr nm <> ":"
                                  <$$+> "Expecting type" <+> ppr t1 <+> "but got" <+> ppr t2
    VarNotFunction nm t           -> "Variable" <+> squotes (ppr nm) <+> "is not a helper function"
                                  <$$+> "Expecting a function type, but got" <+> squotes (ppr t)
    UnificationFail tinfo1 tinfo2 -> "Could not match expected type"
                                  <+> squotes (ppr $ ttype tinfo1)
                                  <+> "with actual type"
                                  <+> squotes (ppr $ ttype tinfo2) <> ":"
                                  <$$+> ppr tinfo1
                                  <$$+> ppr tinfo2
    TransitionOnlyInTrueBranch cond
      -> "A transition occurs when the condition" <+> squotes (ppr cond)
        <+> "of the if-statement is true,"
        <$$+> "but there is no else-branch that specifies where to transition to when the condition is false."
    TransitionOnlyInOneBranch
      -> "A transition occurs in one branch of the if-statement, but not in the other."
    UnreachableStatement loc
      -> "This statement transitions, leaving the following statement at"
        <+> ppr loc <+> "unreachable."
    ExpectedStatement ty
      -> "Expected a statement but got something of type" <+> squotes (ppr ty)
    MethodUnspecifiedTransition name
      -> "Method" <+> squotes (ppr name) <+> "does not specify where to transition to."
        <$$+> "If the method should not transition, use the" <+> squotes (ppr Stay <> "()")
        <+> "primop."
    CaseOnNotEnum e               -> "Case analysis on a non-enum type:"
                                  <$$+> ppr e
    UnknownConstructor c          -> "Reference to undefined constructor:"
                                  <$$+> ppr c
    UnknownEnum e                 -> "Reference to undefined enum type:"
                                  <$$+> ppr e
    PatternMatchError misses dups -> "Pattern match failures:"
                                  <$$+> vsep (map ((" - Missing case for:" <+>) . ppr) misses)
                                  <$$+> vsep (map ((" - Duplicate case for:" <+>) . ppr) dups)
    EmptyMatches                  -> "Case expression with no matches"
    InvalidAccessRestriction e t  -> ("Invalid access restriction:" <$$+> ppr e)
                                  <$$> "Expected type"
                                  <+> ppr TAccount
                                  <+> "but got"
                                  <+> ppr t
    Impossible msg                -> "The impossible happened:" <+> ppr msg
    UnknownHoleInProgram          -> "Expecting an expression or statement here."
    HoleInProgram{ inferredType, suggestions }
      -> "Expecting something of type" <+> sqppr inferredType <> "."
      <$$+> (notNull suggestions ? "Suggestions:"
            <$$+> vsep (map pprSuggestion suggestions))
        where
          pprSuggestion (name, (tmeta, _)) = ppr tmeta <+> ppr name
          notNull = not . null

instance (Eq as, Eq ac, Eq c, Pretty as, Pretty ac, Pretty c) => Pretty (TypeError as ac c) where
  ppr (TypeError h@HoleInProgram{} loc)
    = "Hole at" <+> ppr loc <> ":"
        <$$+> ppr h
  ppr (TypeError h@UnknownHoleInProgram loc)
    = "Hole at" <+> ppr loc <> ":"
        <$$+> ppr h
  ppr (TypeError tErrInfo tPos) = "Error at" <+> ppr tPos <> ":"
                             <$$+> ppr tErrInfo

instance (Ord as, Ord ac, Ord c, Pretty as, Pretty ac, Pretty c) => Pretty [TypeError as ac c] where
  ppr es = case map ppr (sort es) of
    [] -> ""
    (e:es) -> foldl' (<$$$>) (ppr e) $ map ppr es

instance (Ord as, Ord ac, Ord c, Pretty as, Pretty ac, Pretty c) => Pretty (NonEmpty (TypeError as ac c)) where
  ppr = ppr . NonEmpty.toList

-- | Pretty print a type error
ppError :: (Eq as, Eq ac, Eq c, Pretty as, Pretty ac, Pretty c) => TypeError as ac c -> LText
ppError = render . ppr

-- | Pretty print a type signature
ppSig :: Sig -> LText
ppSig = render . ppr
