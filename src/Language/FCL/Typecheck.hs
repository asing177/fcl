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

module Language.FCL.Typecheck (
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
  createADTInfo,

  -- ** Pretty Printing
  ppSig,
  ppError,
) where

import Protolude hiding (Type, TypeError, Constraint)
import Unsafe (unsafeIndex)
import Numeric.Lossless.Number
import Language.FCL.AST
import Language.FCL.Prim
import Language.FCL.Pretty hiding ((<>))
import qualified Language.FCL.Token as Token (case_)
import Language.FCL.Utils ((?), zipWith3M_)

import Test.QuickCheck
import Generic.Random
import Control.Monad.State.Strict (modify')
import Data.Aeson as A hiding (Value)
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
data Sig = Sig
  { sigTypes :: [Type]
  , sigType :: Type
  } deriving (Eq, Show, Generic, Serialize, ToJSON, FromJSON)

-- | Type error metadata
-- This type uses ByteString for ad-hoc error messages so that we can get a
-- Serialize instance for free.
data TypeErrInfo
  = UnboundVariable Name                -- ^ Unbound variables
  | Shadow Name TMeta TypeInfo          -- ^ Shadowing variables/methods/helpers
  | InvalidDefinition Name Expr Type Type -- ^ Invalid definition
  | InvalidUnOp UnOp Type               -- ^ Invalid unary op
  | InvalidBinOp BinOp Type Type        -- ^ Invalid binary op
  | UndefinedFunction Name              -- ^ Invocation of non-existent non-primop function
  | InvalidAddress Text                 -- ^ Invalid address
  | InvalidArgType Name Type Type       -- ^ Invalid argument to Method call
  | VarNotFunction Name Type         -- ^ Helpers must be of type 'TFun'
  | ArityFail Name Int Int              -- ^ Incorrect # args supplied to function
  | UnificationFail TypeInfo TypeInfo   -- ^ Unification fail
  | TransitionOnlyInTrueBranch LExpr    -- ^ Transition in an @else@less @if@ statement
  | TransitionOnlyInOneBranch           -- ^ Transition only in one branch of @if@ statement
  | UnreachableStatement Loc            -- ^ The following statement is unreachable because of a transition
  | ExpectedStatement Type              -- ^ Expected statement but got expression
  | ExpectedExpressionAssRHS LExpr      -- ^ Expected expression but got statement
  | MethodUnspecifiedTransition Name    -- ^ Method doesn't specify where to transition to
  | CaseOnNotADT TypeInfo              -- ^ Case analysis on non-adt type
  | UnknownConstructor NameUpper        -- ^ Reference to undefined constructor
  | UnknownADT Name                     -- ^ Reference to unknown adt type
  | EmptyMatches                        -- ^ Case statement with empty matches
  | PatternMatchError
    { patMatchErrorMissing :: [ADTConstr]
    , patMatchErrorDuplicate :: [ADTConstr]
    }                                   -- ^ Pattern match failures
  | Impossible Text -- ^ Malformed syntax, impossible
  | UnknownHoleInProgram
    -- ^ There is an underconstrained hole, e.g. @z = ?@ where @z@ is a temp variable
  | HoleInProgram
    { inferredType :: Type, suggestions :: [(Name, (TMeta, TypeInfo))] }
    -- ^ There is a hole in the program, so we give the context
  | InvalidPrecision
  | NumPrecisionMismatch
    { typeInfo1 :: TypeInfo
    , typeInfo2 :: TypeInfo
    }
  | TooManyPatterns NameUpper Pattern
  | TooManyArguments NameUpper LExpr
  | NotEnoughArguments NameUpper (LName, Type)
  | FieldTypeMismatch (NonEmpty (LName, Type))
  | FieldAccessOnNonADT TypeInfo
  | InvalidField{ tyErrField :: Name, tyErrTyName :: Name, validFields :: [(Name, Type)] }
  | ExpectedField { tyErrTyName :: Name }
  deriving (Eq, Show, Generic, Serialize, A.FromJSON, A.ToJSON)

instance Arbitrary TypeErrInfo where
  arbitrary = genericArbitraryU

-- | Type error
data TypeError = TypeError
  { errInfo :: TypeErrInfo
  , errLoc  :: Loc
  } deriving (Eq, Show, Generic, Serialize, A.FromJSON, A.ToJSON)

instance Arbitrary TypeError where
  arbitrary = TypeError <$> arbitrary <*> arbitrary

instance Ord TypeError where
  compare te1 te2 = compare (errLoc te1) (errLoc te2)

-- | Source of type
--
-- TODO Since there are functions (helpers) defined now, error messages could be
-- improved by attaching location information to arguments of functions. This
-- way, when a type origin is assigned to the type of an argument to a function,
-- the origin will point directly to the column the argument name is introduced,
-- instead of the column that the helper function name occurs in.
data TypeOrigin
  = OutOfThinAir
  | ExpectedFromMethodBody -- ^ Expected type from method body (i.e. @TTransition@)
  | ExpectedFromSeq -- ^ Expected type of left-hand side of @ESeq@
  | VariableDefn Name     -- ^ Top level definitions
  | InferredFromVar Name  -- ^ Local method variable assignment
  | InferredFromExpr Expr -- ^ Local method variable assignment
  | InferredFromLit Lit   -- ^ Literal types
  | InferredFromAssetType Name Type -- ^ Holdings type inferred from asset type passed to primop
  | InferredFromHelperDef Name -- ^ Inferred from helper function definition
  | InferredFromCollType Name TCollection -- ^ Inferred from collection type in collection primop
  | InferredFromAssignment (NonEmpty Name) -- ^ Inferred from assignment to existing variable
  | InferredFromMethodBody -- ^ Inferred from method body of given method
  | UnaryOperator UnOp    -- ^ From unary operation
  | BinaryOperator BinOp  -- ^ From binary operation
  | IfCondition           -- ^ From if condition
  | DateTimeGuardPred     -- ^ From DateTime guard predicate
  | DateTimeGuardBody     -- ^ From DateTime guard body
  | Assignment            -- ^ From var assignment
  | FunctionArg Int Name  -- ^ Expr passed as function argument + it's position
  | FunctionRet Name      -- ^ Returned from prip op
  | FromConstructor NameUpper -- ^ ADT type of pattern
  | FromConstructorField { tyOrigConstructor :: NameUpper, tyOrigField :: Name }
  | CaseBody Expr         -- ^ Body of case match
  | FromCase Loc
  | FromVariablePattern LName
  | EmptyCollection       -- ^ From an empty collection
  | MapExpr               -- ^ From a Map expression
  | SetExpr               -- ^ From a Set expression
  | FromPrecondition Precondition -- ^ From a method precondition
  | FromHole
  | EmptyBlock -- ^ An empty block has type @TVoid@
  | FromRoundingPrecision -- ^ The output precision of a rounding operation
  | FromRecordAccess { tyOrigField :: Name, tyOrigTyName :: Name }
  deriving (Eq, Ord, Show, Generic, Serialize, A.FromJSON, A.ToJSON)

instance Arbitrary TypeOrigin where
  arbitrary = genericArbitraryU

-- | Type error metadata
data TypeInfo = TypeInfo
  { ttype :: Type        -- ^ What type
  , torig :: TypeOrigin  -- ^ Where did it come from
  , tloc  :: Loc         -- ^ Where is it located
  } deriving (Show, Eq, Ord, Generic, Serialize, A.FromJSON, A.ToJSON)

instance Arbitrary TypeInfo where
  arbitrary = genericArbitraryU

tContractInfo, tBoolInfo, tAccountInfo, tDatetimeInfo, tDeltaInfo, tMsgInfo :: TypeOrigin -> Loc -> TypeInfo
tContractInfo = TypeInfo TContract
tBoolInfo = TypeInfo TBool
tAccountInfo = TypeInfo TAccount
tDatetimeInfo = TypeInfo TDateTime
tDeltaInfo = TypeInfo TTimeDelta
tMsgInfo = TypeInfo TText

tAssetInfo :: Type -> TypeOrigin -> Loc -> TypeInfo
tAssetInfo = TypeInfo . TAsset

tFunInfo :: [Type] -> TVar -> TypeOrigin -> Loc -> TypeInfo
tFunInfo argTypes tv = TypeInfo (TFun argTypes (TVar tv))

throwErrInferM :: TypeErrInfo -> Loc -> InferM TypeInfo
throwErrInferM tErrInfo loc = do
    modify' $ \inferState ->
      inferState { errs = errs inferState ++ [typeError] }
    return $ TypeInfo TError OutOfThinAir loc
  where
    typeError = TypeError tErrInfo loc

throwTypeErrInferM :: TypeError -> InferM TypeInfo
throwTypeErrInferM (TypeError err loc) = throwErrInferM err loc

data Hole = Hole
  { holeLocation :: Loc
  , holeContext  :: Context (TMeta, TypeInfo)
  , holeTyVar    :: TVar
  } deriving (Eq, Ord, Show)


-- | Type inference monad state
data InferState = InferState
  { count       :: Int            -- ^ unique counter for variable freshening
  , errs        :: [TypeError]    -- ^ accumulator for type errors
  , constraints :: [Constraint]   -- ^ accumulator for type equality constraints
  , holes       :: Set Hole       -- ^ accumulator for holes
  , context     :: Context (TMeta, TypeInfo)  -- ^ variable context
  } deriving (Show)

emptyInferState :: InferState
emptyInferState = InferState 0 mempty mempty mempty emptyContext

data TMeta = Global | Temp | MethArg | HelperArg | HelperFunc | PatternVar
  deriving (Show, Eq, Ord, Generic, Serialize, A.FromJSON, A.ToJSON)

instance Arbitrary TMeta where
  arbitrary = genericArbitraryU

instance Pretty TMeta where
  ppr = \case
    Global -> "global variable"
    Temp -> "temporary variable"
    MethArg -> "method argument"
    HelperArg -> "helper argument"
    HelperFunc -> "helper function"
    PatternVar -> "variable bound by a pattern"
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
extendContextInferM :: TMeta -> (Name, TypeInfo) -> InferM ()
extendContextInferM meta (id, info) = do
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
bracketScopeTmpEnvM :: InferM a -> InferM a
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


type InferM = ReaderT ADTInfo (State InferState)

runInferM
  :: ADTInfo -> InferState -> InferM a -> (a, InferState)
runInferM adtInfo inferState act
  = runState (runReaderT act adtInfo) inferState


--------------------------------------------------------------------------------
-- ADT Information
--------------------------------------------------------------------------------

-- | Create the dictionaries for the constructor/adt type membership
-- relations from the original list of definitionSet. Assumes that there
-- are no duplicates in the input.
createADTInfo :: [ADTDef] -> Either (NonEmpty TypeError) ADTInfo
createADTInfo adts = ADTInfo constructorToType <$> adtToConstrsAndFields
  where
    constructorToType :: Map NameUpper (LName, [(LName, Type)])
    constructorToType = Map.fromList . concatMap go $ adts
      where
        go (ADTDef lname constrs) =
          map
            (\(ADTConstr id types) -> (locVal id, (lname, types)))
          . toList
          $ constrs

    adtToConstrsAndFields :: Either (NonEmpty TypeError) (Map Name (NonEmpty ADTConstr, [(Name, Type)]))
    adtToConstrsAndFields = Map.fromList <$> mapM go adts
      where
        go :: ADTDef -> Either (NonEmpty TypeError) (Name, (NonEmpty ADTConstr, [(Name, Type)]))
        go (ADTDef (Located _ tyName) constructors) = do
            fields <- getValidFields constructors
            pure (tyName, (constructors, fields))

-- | Get the valid field names of a data type definition. Fields are valid
-- exactly when they appear in each constructor. The same field appearing at
-- different types within one type constructor is an error. Example:
-- For @type T = C1(int x, int y) | C2(int x);@ the only valid field is @int x@.
-- Assumes that 'Located' equality and ordering ignores the location (which is
-- the case at the time of this writing).
getValidFields :: NonEmpty ADTConstr -> Either (NonEmpty TypeError) [(Name, Type)]
getValidFields cs = case partitionFields cs of
    ([], validFields) -> Right . map (first locVal) $ validFields
    (xs:xss, _) -> Left (map (flip TypeError NoLoc . FieldTypeMismatch) (xs :| xss))
  where
    -- Get the fields of a constructor and partition them into the ones where a
    -- field is  (left) and
    -- the good (right).
    partitionFields :: NonEmpty ADTConstr -> ([NonEmpty (LName, Type)], [(LName, Type)])
    partitionFields cs
      = second (concatMap toList)         -- flatten both sides of the pair
      . List.partition ((> 1) . length)   -- if the length of a group is >1, there is a conflict
      . map NonEmpty.nub                  -- throw out duplicates
      . filter ((== length cs) . length)  -- only keep fields which are present in every constructor
      . NonEmpty.groupBy ((==) `on` fst)  -- group by field name
      . sortOn fst                        -- sort on field name
      . sconcat                           -- concatenate into a single list
      . map adtConstrParams               -- get the fields
      $ cs

-------------------------------------------------------------------------------
-- Type Signatures for Methods
-------------------------------------------------------------------------------

-- | Typechecks whether the values supplied as arguments to the method
-- call match the method argument types expected
tcMethodCall :: ADTInfo -> Method -> [Value] -> Either TypeErrInfo ()
tcMethodCall adtInfo method argVals
  = do
  actualTypes <- mapM valueType argVals
  zipWithM_ validateTypes expectedTypes actualTypes
  where
    valueType :: Value -> Either TypeErrInfo Type
    valueType val
      = case (val, mapType adtInfo val) of
          (_, Just ty) -> pure ty
          (VConstr c _, Nothing) -> Left $ UnknownConstructor c
          (o, Nothing) -> Left $ Impossible $ "Malformed value: " <> show o

    expectedTypes = Language.FCL.AST.argtys' method

    validateTypes expected actual =
      when (not $ validMethodArgType expected actual) $
        Left $ InvalidArgType (locVal $ methodName method) expected actual

    validMethodArgType :: Type -> Type -> Bool
    validMethodArgType (TAsset _) (TAsset TAny) = True
    validMethodArgType (TAsset TAny) (TAsset _) = True
    validMethodArgType (TNum np1) (TNum np2) = np1 <= np2
    validMethodArgType t1 t2 = t1 == t2

signatures :: Script -> Either (NonEmpty TypeError) [(Name,Sig)]
signatures (Script adts defns graph methods helpers) =
    case createADTInfo adts of
      Left err -> Left err
      Right adtInfo -> do
        let inferState = snd . runInferM adtInfo emptyInferState
              $ tcDefns defns *> tcHelpers helpers
            (tcErrs, methodSigs) = partitionEithers
              $ map (methodSig adtInfo inferState) methods
        case tcErrs of
          []   -> Right methodSigs
          es:ess -> Left . NonEmpty.nub . sconcat $ es:|ess

-- | Typechecks a top-level 'Method' function body and returns a type signature
-- This type 'Sig' differs from helper functions because there is a distinct
-- difference between helper functions and methods. Methods are *not* callable
-- from other methods or functions, whereas helper functions are able to be
-- called from any method or other helper functions.
methodSig
  :: ADTInfo
  -> InferState
  -> Method
  -> Either (NonEmpty TypeError) (Name, Sig)
methodSig adtInfo initInferState m@(Method _ precs lMethNm args body) =
    case runSolverM resInferState of
      Right _ -> Right (locVal lMethNm, sig)
      Left es -> Left es
  where
    -- Typecheck body of method, generating constraints along the way
    (sig, resInferState) = runInferM adtInfo initInferState $ do
        tcPreconditions precs
        sig@(Sig _argTys typ) <- functionSig MethArg (locVal lMethNm, args, body)
        unless (typ == TTransition || typ == TError)
              (void $ throwErrInferM (MethodUnspecifiedTransition (locVal lMethNm)) (located body))
        pure sig

tcHelpers :: [Helper] -> InferM ()
tcHelpers helpers =
  forM_ helpers $ \helper -> do
    helperInfo <- tcHelper helper
    extendContextInferM HelperFunc (locVal (helperName helper), helperInfo)

-- | Typechecks a 'Helper' function body and returns a TypeInfo of TFun,
-- representing the type of a function. This differs from the 'Sig' value
-- returned, because helper functions are injected into the variable environment
-- as variables with 'TFun' types.
tcHelper
  :: Helper
  -> InferM TypeInfo
tcHelper (Helper fnm args body) = do
  (Sig argTypes retType) <- functionSig HelperArg (locVal fnm, args, body)
  let tfun = TFun argTypes retType
  pure (TypeInfo tfun (InferredFromHelperDef (locVal fnm)) (located fnm))

-- | Gives a function signature to function-esque tuples:
-- Given a triple of a function name, a list of arguments and an expression
-- find the type signature of the expression body (using some initial state)
functionSig
  :: TMeta
  -> (Name, [Arg], LExpr)
  -> InferM Sig
functionSig meta (fnm, args, body) = do
  argInfos <- zipWithM (tcArg fnm) args [1..]
  bracketScopeTmpEnvM $ do
    mapM_ (extendContextInferM meta) argInfos
    retType <- tcLExprScoped body
    pure $ Sig (map argType args) (ttype retType)

-- | Given a type, if that type is an undeclared ADT type, return error 'TypeInfo'.
adtExistsCheck :: Loc -> Type -> InferM (Maybe TypeInfo)
adtExistsCheck loc (TADT nm) = do
    adtDoesExist <- Map.member nm . adtToConstrsAndFields <$> ask
    if adtDoesExist then pure Nothing else Just <$> throwErrInferM (UnknownADT nm) loc
adtExistsCheck _ _ = pure Nothing

-- | Typechecks the argument and returns the type info of the function argument
-- such that it can be added to the typing env in the manner the caller prefers.
tcArg :: Name -> Arg -> Int -> InferM (Name, TypeInfo)
tcArg fnm (Arg typ (Located loc anm)) argPos = do
    adtExistsCheck loc typ >>= \case
      Nothing -> pure (anm, TypeInfo typ (FunctionArg argPos fnm) loc)
      Just err -> pure (anm, err)

-------------------------------------------------------------------------------
-- Typechecker (w/ inference)
-------------------------------------------------------------------------------

tcDefn :: Def -> InferM ()
tcDefn def = extendContextInferM Global =<< case def of
  -- GlobalDef variable can be any type
  GlobalDefNull typ precs (Located loc nm) -> do
    tcPreconditions precs
    adtExistsCheck loc typ >>= \case
      Nothing -> pure (nm, TypeInfo typ (VariableDefn nm) loc)
      Just err -> pure (nm, err)

  GlobalDef typ precs nm lexpr -> do
    tcPreconditions precs
    exprTypeInfo@(TypeInfo _ _ loc) <- tcLExpr lexpr
    adtExistsCheck loc typ >>= \case
      Just err -> pure (nm, err)
      Nothing -> do
        let typeInfo = TypeInfo typ (VariableDefn nm) loc
        case unifyDef nm lexpr typeInfo exprTypeInfo of
          Left terr -> void $ throwErrInferM terr loc
          Right _   -> pure ()
        pure (nm, typeInfo)
  where
    -- Check if the stated definition type and the rhs expr type match
    unifyDef :: Name -> LExpr -> TypeInfo -> TypeInfo -> Either TypeErrInfo ()
    unifyDef nm le t1 t2 = do
      case runSolverM emptyInferState{ constraints = [Constraint (Just le) t1 t2] } of
        Right _ -> pure ()
        Left (terr :| _) ->
          case errInfo terr of
            UnificationFail ti1 ti2 -> Left $ InvalidDefinition nm (locVal le) (ttype ti1) (ttype ti2)
            otherwise               -> panic "Solver should fail with UnificationFail"


tcDefns :: [Def] -> InferM ()
tcDefns = mapM_ tcDefn

-- | Type check an LExpr. Remember to use `bracketScopeTmpEnvM` if temporary
-- variables are meant to be discarded.
tcLExpr :: LExpr -> InferM TypeInfo
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
          addConstraint e1 (TypeInfo TVoid ExpectedFromSeq (located e1)) t1
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
                zipWith3M_ addConstraint argExprs argTypeInfos' argTypeInfos
                pure $ TypeInfo retType (FunctionRet hnm) loc
              -- If error typechecking helper, leave it be
              TypeInfo TError _ _ -> pure tinfo
              -- If any other type, report the error but continue
              TypeInfo varType _ _  -> do
                let terr = VarNotFunction (locVal helperNm) varType
                throwErrInferM terr (located helperNm)

  EAssign nms@(nm :| fds) e -> do
    eTypeInfo@(TypeInfo eType _ eLoc) <- tcLExpr e
    when (eType == TVoid) (void $ throwErrInferM (ExpectedExpressionAssRHS e) eLoc)
    lookupAssignVarType (Located loc nm) fds >>= \case

      Nothing -> do -- New temp variable, instantiate it
        let typeInfo = TypeInfo eType (InferredFromExpr $ locVal e) eLoc
        extendContextInferM Temp (nm, typeInfo)

      -- can only assign to mutable variable, so need to check this here
      Just (meta, varTypeInfo) -> if meta == Global || meta == Temp || meta == HelperArg
        then do
          tvar <- freshTVar
          let retTypeInfo = TypeInfo tvar (InferredFromAssignment nms) (located e)
          addConstraint e varTypeInfo retTypeInfo
          addConstraint e retTypeInfo eTypeInfo
        else void $ throwErrInferM (Shadow nm meta varTypeInfo) loc

    return $ TypeInfo TVoid Assignment (located e)

  EBefore edt e -> do
    dtTypeInfo <- tcLExpr edt
    eTypeInfo  <- tcLExprScoped e
    addConstraint edt (tDatetimeInfo DateTimeGuardPred loc) dtTypeInfo
    addConstraint e (TypeInfo TVoid DateTimeGuardBody loc) eTypeInfo
    return $ TypeInfo TVoid DateTimeGuardBody loc

  EAfter edt e -> do
    dtTypeInfo <- tcLExpr edt
    eTypeInfo  <- tcLExprScoped e
    addConstraint edt (tDatetimeInfo DateTimeGuardPred loc) dtTypeInfo
    addConstraint e (TypeInfo TVoid DateTimeGuardBody loc) eTypeInfo
    return $ TypeInfo TVoid DateTimeGuardBody loc

  EBetween startDte endDte e -> do
    startTypeInfo <- tcLExpr startDte
    endTypeInfo <- tcLExpr endDte

    let dtInfo = tDatetimeInfo DateTimeGuardPred loc
    addConstraint startDte dtInfo startTypeInfo
    addConstraint endDte dtInfo endTypeInfo

    eTypeInfo  <- tcLExpr e
    addConstraint e (TypeInfo TVoid DateTimeGuardBody loc) eTypeInfo

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
    addConstraint cond cTypeInfo' cTypeInfo
    if (ttype e1TypeInfo == TTransition) `xor` (ttype e2TypeInfo == TTransition)
      then if locVal e2 == ENoOp
        then throwErrInferM (TransitionOnlyInTrueBranch cond) loc
        else throwErrInferM TransitionOnlyInOneBranch loc
      else do
        addConstraint e1 e1TypeInfo e2TypeInfo
        let retTypeInfo = TypeInfo
              { ttype = ttype e1TypeInfo
              , torig = IfCondition
              , tloc  = loc
              }
        return retTypeInfo

  ECase scrut matches -> do
    scrutTypeInfo <- tcLExpr scrut
    bodiesTys <- mapM (\m@(CaseBranch _ e) -> (e,) <$> tcCaseBranch scrutTypeInfo m) matches
    case bodiesTys of
      [] -> throwErrInferM EmptyMatches $ located scrut
      ((bodyExpr, tyInfo):eis) -> do
        mapM_ (addConstraint bodyExpr tyInfo . snd) eis
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

  EConstr constructor es ->
    Map.lookup constructor . constructorToType <$> ask >>= \case
      Nothing -> throwErrInferM (UnknownConstructor constructor) loc
      Just (typeConstr, paramTys) -> do
        econstrZip paramTys es
        pure TypeInfo
          { ttype = TADT (locVal typeConstr)
          , torig = FromConstructor constructor
          , tloc = loc }
    where
      econstrZip
        :: [(LName, Type)] -- ^ the types and names of the constructor fields
        -> [LExpr] -- ^ the constructor arguments
        -> InferM ()
      econstrZip ((field, ty):ts) (e:es) = do
        let tyExpected = TypeInfo
                          ty
                          FromConstructorField{ tyOrigConstructor = constructor, tyOrigField = locVal field }
                          (located e)
        tyActual <- tcLExpr e
        addConstraint e tyExpected tyActual
        econstrZip ts es
      econstrZip [] [] = pure ()
      econstrZip [] (e:es) = throwErrInferM (TooManyArguments constructor e) (located e) *> econstrZip [] es
      econstrZip (t:ts) [] = throwErrInferM (NotEnoughArguments constructor t) loc *> econstrZip ts []

  where
    tcLExprs :: Foldable f => f LExpr -> InferM TypeInfo
    tcLExprs = foldM step zero
      where
        step prevTyInfo e = do
          tyInfo <- tcLExpr e
          addConstraint e tyInfo prevTyInfo
          pure tyInfo
        zero = TypeInfo TAny EmptyCollection loc

-- | Type check an LExpr in a new scope which doesn't leak temporary variables.
tcLExprScoped :: LExpr -> InferM TypeInfo
tcLExprScoped = bracketScopeTmpEnvM . tcLExpr

-- | Type check 'CaseBranch' patterns (lhs) against the scrutinee type and return the
-- expression type (rhs).
tcCaseBranch
  :: TypeInfo -- ^ scrutinee type
  -> CaseBranch -- ^ a branch of a case statement
  -> InferM TypeInfo -- ^ return the type of the branch body
tcCaseBranch tyExpected (CaseBranch (Located patLoc pat) body)
  = bracketScopeTmpEnvM $ do
      patContext <- tcPattern tyExpected pat
      mapM_ (extendContextInferM PatternVar) patContext
      tcLExpr body

type PatContext = [(Name, TypeInfo)]

tcPattern
  :: TypeInfo -- ^ the expected type
  -> Pattern -- ^ the pattern to typecheck
  -> InferM PatContext -- ^ the outgoing pattern context (bound variables)
tcPattern tyExpected (PatLit l) = do
    patTy <- tcLLit l
    addConstraint' tyExpected patTy
    pure []

tcPattern tyExpected (PatVar v)
  = pure [(locVal v, TypeInfo (ttype tyExpected) (FromVariablePattern v) (located v))]

tcPattern _ PatWildCard = pure []

tcPattern tyExpected (PatConstr (Located loc c) pats)
  = Map.lookup c . constructorToType <$> ask >>= \case
      Nothing -> do
        throwErrInferM (UnknownConstructor c) loc
        pure []
      Just (typeConstr, paramTys) -> do
        addConstraint' tyExpected TypeInfo
          { ttype = TADT (locVal typeConstr)
          , torig = FromConstructor c
          , tloc = loc }
        patZip paramTys pats []
  where
    patZip
      :: [(LName, Type)] -- ^ the types and names of the constructor fields
      -> [Pattern] -- ^ the constructor argument patterns to check
      -> PatContext -- ^ accumulator of the pattern context (bindings that get created through matches)
      -> InferM PatContext
    patZip ((nm, ty):ts) (p:ps) acc = do
      patCtx <- tcPattern (TypeInfo ty (FromConstructorField c (locVal nm)) (patLoc p)) p
      patZip ts ps (patCtx <> acc)
    patZip [] [] acc = pure acc
    patZip [] (p:ps) acc = throwErrInferM (TooManyPatterns c p) (patLoc p) *> patZip [] ps acc
    patZip (t:ts) [] acc = throwErrInferM (NotEnoughArguments c t) loc *> patZip ts [] acc

tcLLit :: LLit -> InferM TypeInfo
tcLLit (Located loc lit)
  = case tcLit lit of
    Left err  -> throwErrInferM err loc
    Right typ -> pure $
      TypeInfo typ (InferredFromLit lit) loc

tcLit :: Lit -> Either TypeErrInfo Type
tcLit lit =
  case lit of
    LNum (Decimal p v)        -> Right (TNum $ NPDecimalPlaces p)
    LBool _                   -> Right TBool
    LText _                   -> Right TText
    LSig _                    -> Right TSig
    LAccount addr             -> Right TAccount
    LAsset addr               -> Right $ TAsset TAny
    LContract addr            -> Right TContract
    LState label              -> Right TState
    LDateTime _               -> Right TDateTime
    LTimeDelta _              -> Right TTimeDelta

-------------------------------------------------------------------------------
  -- Type checking of prim op calls
-------------------------------------------------------------------------------

tcPrim :: LExpr -> Loc -> PrimOp -> [LExpr] -> InferM TypeInfo
tcPrim le eLoc prim argExprs = do

    -- Setup some prim op agnostic contextironment
    -- 1) Lookup what the type of the arguments of the prim op should be
    -- 2) Typecheck the arg exprs supplied to the prim op call
    -- 3) Create the type infos of the arg types
    (Sig argTypes retType) <- primSig prim
    correctArity <- arityCheck (Located eLoc primNm) argTypes argExprs
    -- Now check the prim (if the arity is correct) and maybe get a new return
    -- type (in the case of a dependent primop)

    let argTypeOrig n = FunctionArg n primNm
        mkArgTypeInfo t (lexpr, n) = TypeInfo t (argTypeOrig n) $ located lexpr
        argTypeInfosExpected = zipWith mkArgTypeInfo argTypes (zip argExprs [1..])
        retTypeInfo = TypeInfo retType (FunctionRet primNm) eLoc

    mbRetTypeInfo <- if correctArity
      then do
        argTypeInfosActual <- mapM tcLExpr argExprs
        zipWith3M_ addConstraint argExprs argTypeInfosExpected argTypeInfosActual
        tcPrimSpecial argTypeInfosExpected argTypeInfosActual retTypeInfo prim
      else pure Nothing
    return (fromMaybe retTypeInfo mbRetTypeInfo)

  where
    primNm = primName prim

    -- The return type of some primops depends on its operands, hence here we
    -- may get a different return type. NB: Assumes that arities have been
    -- checked.
    tcPrimSpecial
      :: [TypeInfo]
      -> [TypeInfo]
      -> TypeInfo
      -> PrimOp
      -> InferM (Maybe TypeInfo)
    tcPrimSpecial argTypeInfosExpected argTypeInfosActual retTypeInfo prim = do

      case prim of
        -- round : (int n, num) -> decimal<n>
        Round -> do
          let [precisionExpr, operandExpr] = argExprs
          --     [_, operandTInfoExpected]    = argTypeInfos
          --     [_, operandTInfoActual]      = argTypeInfosActual

          -- addConstraint operandExpr operandTInfoExpected operandTInfoActual

          -- do the dependently typed magic: get the int literal to be used
          -- in the return type of the rounding operation
          case precisionExpr of
            Located loc (ELit (Located _ (LNum (Decimal 0 n)))) ->
              pure . Just $ TypeInfo
                (TNum (NPDecimalPlaces n))
                FromRoundingPrecision
                loc
            Located loc _ -> do
              _ <- throwTypeErrInferM $ TypeError InvalidPrecision loc
              pure Nothing

        -- Type checking for `Round(Down|Up)` is the same as for `Round`
        RoundDown -> tcPrimSpecial argTypeInfosExpected argTypeInfosActual retTypeInfo Round
        RoundUp -> tcPrimSpecial argTypeInfosExpected argTypeInfosActual retTypeInfo Round

        -- AssetPrimOps must be typechecked uniquely-- Sometimes the type of one of
        -- the arguments to the prim op is dependent on the type of the asset
        -- supplied, and other times the _return type_ is dependent on the type of
        -- the asset supplied  as an argument.
        AssetPrimOp assetPrimOp -> do
          case assetPrimOp of

            HolderBalance    -> do
              let [assetVarExpr, accExpr]  = argExprs
                  [tassetVarInfo,taccInfo] = argTypeInfosExpected
                  [tassetInfo,taccInfo']   = argTypeInfosActual
              -- add constraint for 1st arg to be an asset type
              addConstraint assetVarExpr tassetVarInfo tassetInfo
              -- add constraint for 2nd arg to be an account type
              addConstraint accExpr taccInfo taccInfo'

            TransferHoldings -> do
              let [accExpr, assetVarExpr, varExpr, acc2Expr]      = argExprs
                  [tacc1Info, tassetVarInfo, tvarInfo, tacc2Info] = argTypeInfosExpected
                  [tacc1Info', tassetInfo, tbalInfo, tacc2Info']  = argTypeInfosActual
              -- add constraint for 1st arg to be an account
              addConstraint accExpr tacc1Info tacc1Info'
              -- add constraint for 2nd arg to be an asset
              addConstraint assetVarExpr tassetVarInfo tassetInfo
              -- add constraint for 3rd arg depending on asset type
              addConstraint varExpr tvarInfo tbalInfo
              -- add constraint for 4th arg to be an account
              addConstraint acc2Expr tacc2Info tacc2Info'

            TransferTo       -> do
              let [assetVarExpr, varExpr]  = argExprs
                  [tassetVarInfo, tvarInfo] = argTypeInfosExpected
                  [tassetInfo, tbalInfo]    = argTypeInfosActual
              -- add constraint for 1st arg to be an asset
              addConstraint assetVarExpr tassetVarInfo tassetInfo
              -- add constraint for 2nd arg depending on asset type
              addConstraint varExpr tvarInfo tbalInfo

            TransferFrom     -> do
              let [assetVarExpr, varExpr, accExpr]    = argExprs
                  [tassetVarInfo, tvarInfo, taccInfo] = argTypeInfosExpected
                  [tassetInfo, tbalInfo, taccInfo']   = argTypeInfosActual
              -- add constraint for 1st arg to be an asset
              addConstraint assetVarExpr tassetVarInfo tassetInfo
              -- add constraint for 2nd arg depending on asset type
              addConstraint varExpr tvarInfo tbalInfo
              -- add constraint for 3rd arg to be an account
              addConstraint accExpr taccInfo taccInfo'

            CirculateSupply -> do
              let [assetVarExpr, varExpr]  = argExprs
                  [tassetVarInfo, tvarInfo] = argTypeInfosExpected
                  [tassetInfo, tbalInfo]    = argTypeInfosActual
              -- add constraint for 1st arg to be an asset
              addConstraint assetVarExpr tassetVarInfo tassetInfo
              -- add constraint for 2nd arg depending on asset type
              addConstraint varExpr tvarInfo tbalInfo
          pure Nothing

        CollPrimOp collPrimOp -> do

          -- However, we do some ad-hoc typechecking depending on the collection
          -- prim-op and which collection it's operating over.
          case collPrimOp of
            Aggregate -> do
              let [_, accumExpr, _]  = argExprs
                  [_, tinfoAccum, _] = argTypeInfosExpected
                  [_, _, tinfoColl'] = argTypeInfosActual
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
                  [tinfoFunc, _]  = argTypeInfosExpected
                  [tinfoFunc', tinfoColl'] = argTypeInfosActual
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
                  [tinfoFunc, _]  = argTypeInfosExpected
                  [tinfoFunc', tinfoColl'] = argTypeInfosActual
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
                  [tinfoVal, _] = argTypeInfosExpected
                  [_, tinfoColl'] = argTypeInfosActual
              case tinfoColl' of
                TypeInfo (TColl tcoll) torigCol tlocCol -> do
                  let expectedValType =
                        case tcoll of
                          TMap k _ -> k
                          TSet vType   -> vType
                      tinfoValExpected = TypeInfo expectedValType (InferredFromCollType primNm tcoll) tlocCol
                  -- Add a constraint for the type of value in question to match the type of values
                  -- in the collection
                  addConstraint valExpr tinfoValExpected tinfoVal
                -- If the type is not a function type, don't do anything as it
                -- will fail with a unification error later phase.
                TypeInfo t torig tloc -> pure ()

            -- There is no special typechecking to do here
            IsEmpty -> pure ()

          pure Nothing


        -- All other primops are typechecked simply-- The expressions supplied as
        -- arguments must unify with the types of the arguments denoted in the prim
        -- op signature.
        normalPrimOp -> pure Nothing



    -----------------------------------------------------
    -- Helpers for Constraint Gen for Collection PrimOps
    -----------------------------------------------------

    -- The first, second, and first argument of the HO functions aggregate,
    -- transform, and filter must match the type of the values in the collection.
    addHofArgConstr :: LExpr -> (TypeOrigin, Loc) -> TypeInfo -> Type -> InferM ()
    addHofArgConstr hofExpr (hofTypeOrig, hofTypeLoc) tinfoColl argType =
      case tinfoColl of
        TypeInfo (TColl tcoll) _ tlocCol -> do
          let tinfoCollVals vType = TypeInfo vType (InferredFromCollType primNm tcoll) tlocCol
          -- Depending on the collection type, diff constraints are generated...
          case tcoll of
            -- For maps, the value type must unify with the second function arg type.
            TMap _ vType -> addConstraint hofExpr (tinfoCollVals vType) tinfoArg
            TSet vType   -> addConstraint hofExpr (tinfoCollVals vType) tinfoArg
        -- If the type is not a collection type, don't do anything as it
        -- will fail with a unification error later phase.
        TypeInfo t torig tloc -> pure ()
      where
        tinfoArg = TypeInfo argType hofTypeOrig hofTypeLoc

    -- Add a constraint on the return type of the prim op to be the new type of
    -- the collection after applying the 'transform' primop.
    addRetTypeConstr :: LExpr -> (TypeOrigin, Loc) -> TypeInfo -> Type -> TypeInfo -> InferM ()
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
          addConstraint hofExpr retTypeInfo retCollTypeInfo
        -- If the type is not an collection type, don't do anything as
        -- it will fail with a unification error.
        TypeInfo t torig tloc -> pure ()

arity :: PrimOp -> Int
arity p = case runInferM (ADTInfo mempty mempty) emptyInferState (primSig p) of
  (Sig ps _, _) -> length ps

-- | Type signatures of builtin primitive operations.
primSig :: PrimOp -> InferM Sig
primSig = \case
  Verify              -> pure $ Sig [TAccount, TText, TSig] TBool
  Sign                -> pure $ Sig [TText] TSig
  Block               -> pure . Sig [] . TNum $ nPInt
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
  ContractValue       -> Sig [TContract, TText] <$> freshTVar
  ContractValueExists -> pure $ Sig [TContract, TText] TBool
  ContractState       -> pure $ Sig [TContract] TState
  IsBusinessDayUK     -> pure $ Sig [TDateTime] TBool
  NextBusinessDayUK   -> pure $ Sig [TDateTime] TDateTime
  IsBusinessDayNYSE   -> pure $ Sig [TDateTime] TBool
  NextBusinessDayNYSE -> pure $ Sig [TDateTime] TDateTime
  Between             -> pure $ Sig [TDateTime, TDateTime, TDateTime] TDateTime
  TimeDiff            -> pure $ Sig [TDateTime, TDateTime] TTimeDelta
  Round               -> pure $ Sig [TNum nPInt, TNum NPArbitrary] (panic "dependent return type")
  RoundUp             -> pure $ Sig [TNum nPInt, TNum NPArbitrary] (panic "dependent return type")
  RoundDown           -> pure $ Sig [TNum nPInt, TNum NPArbitrary] (panic "dependent return type")
  RoundRem            -> pure . Sig [TNum nPInt, TNum NPArbitrary] . TNum $ NPArbitrary
  RoundUpRem          -> pure . Sig [TNum nPInt, TNum NPArbitrary] . TNum $ NPArbitrary
  RoundDownRem        -> pure . Sig [TNum nPInt, TNum NPArbitrary] . TNum $ NPArbitrary
  AssetPrimOp a       -> assetPrimSig a
  MapPrimOp m         -> mapPrimSig m
  SetPrimOp m         -> setPrimSig m
  CollPrimOp c        -> collPrimSig c

assetPrimSig :: AssetPrimOp -> InferM Sig
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

mapPrimSig :: MapPrimOp -> InferM Sig
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

setPrimSig :: SetPrimOp -> InferM Sig
setPrimSig = \case
  SetInsert -> do
    a <- freshTVar
    pure $ Sig [a, TColl (TSet a)] (TColl (TSet a))
  SetDelete -> do
    a <- freshTVar
    pure $ Sig [a, TColl (TSet a)] (TColl (TSet a))

collPrimSig :: CollPrimOp -> InferM Sig
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

tcUnOp :: LUnOp -> LExpr -> InferM TypeInfo
tcUnOp (Located opLoc op) e = do
    tcUnOp' opLoc (UnaryOperator op) e
  where
    tcUnOp' =
      case op of
        Not -> tcNotOp

tcBinOp :: LBinOp -> LExpr -> LExpr -> InferM TypeInfo
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
        RecordAccess -> tcRecordAccess

-- -- | Helper for common pattern "Add constraint of two TypeInfos and return a TypeInfo"
-- addConstrAndRetInfo :: LExpr -> TypeInfo -> (TypeInfo, TypeInfo) -> InferM TypeInfo
-- addConstrAndRetInfo le retInfo (expected, actual) =
--   addConstraint le expected actual >> return retInfo

-- | Type check multiplication
tcMult :: Loc -> TypeOrigin -> LExpr -> LExpr -> InferM TypeInfo
tcMult opLoc torig e1 e2 = do
  tinfo1 <- tcLExpr e1
  tinfo2 <- tcLExpr e2
  case (ttype tinfo1, ttype tinfo2) of
    (TNum p1, TNum p2) -> pure $ TypeInfo (TNum (NPAdd p1 p2)) torig eLoc

    -- The reason for these alternating pattern matches is to correctly
    -- typecheck unusual binops in which both expression the operation is over
    -- do not need to necessarily have the same type, albeit a specific one.
    (TTimeDelta, _)   -> addConstrAndRetInfo' (tDeltaInfo torig eLoc) (TypeInfo (TNum nPInt) torig opLoc, tinfo2)
    (_, TTimeDelta)   -> addConstrAndRetInfo' (tDeltaInfo torig eLoc) (tinfo1, TypeInfo (TNum nPInt) torig opLoc)
    (TVar a, _)       -> addConstrAndRetInfo' tinfo1 (tinfo1, tinfo2)
    (_, TVar a)       -> addConstrAndRetInfo' tinfo1 (tinfo1, tinfo2)
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
tcAddSub :: BinOp -> Loc -> TypeOrigin -> LExpr -> LExpr -> InferM TypeInfo
tcAddSub op opLoc torig e1 e2 = do
  tinfo1 <- tcLExpr e1
  tinfo2 <- tcLExpr e2
  case (ttype tinfo1, ttype tinfo2) of
    (TNum p1, TNum p2) -> pure $ TypeInfo (TNum (NPMax p1 p2)) torig eLoc
    -- Constrain the LHS to be the only type expected in the binary operation
    -- that it can be, due to binary op definitions in FCL.
    (TDateTime, _)    -> addConstrAndRetInfo' (tDatetimeInfo torig eLoc) (tDeltaInfo torig opLoc, tinfo2)
    -- TDelta + TDelta (no subtraction)
    (TTimeDelta, _)   -> tcAddNoSub tinfo1 tinfo2
    -- TMsg + TMsg (concatenation, no subtraction)
    (TText, _)         -> tcAddNoSub tinfo1 tinfo2
    (TVar a, _)       -> addConstrAndRetInfo' tinfo1 (tinfo1, tinfo2)
    (_, TVar a)       -> addConstrAndRetInfo' tinfo1 (tinfo1, tinfo2)
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
tcDiv :: Loc -> TypeOrigin -> LExpr -> LExpr -> InferM TypeInfo
tcDiv opLoc torig e1 e2 = do
  tinfo1 <- tcLExpr e1
  tinfo2 <- tcLExpr e2
  case (ttype tinfo1, ttype tinfo2) of
    (TNum _, TNum _)  -> pure $ TypeInfo (TNum NPArbitrary) torig eLoc
    (TVar a, _)       -> addConstrAndRetInfo' tinfo1 (tinfo1, tinfo2)
    (_, TVar a)       -> addConstrAndRetInfo' tinfo1 (tinfo1, tinfo2)
    (t1,t2)           -> do
      throwErrInferM (InvalidBinOp Div t1 t2) opLoc
      return $ TypeInfo TError torig eLoc
  where
    eLoc = located e1
    addConstrAndRetInfo' = addConstrAndRetInfo e2

tcAndOr :: Loc -> TypeOrigin -> LExpr -> LExpr -> InferM TypeInfo
tcAndOr opLoc torig e1 e2 = do
  tinfo1 <- tcLExpr e1
  tinfo2 <- tcLExpr e2
  let argTypeInfo = TypeInfo TBool torig opLoc
  addConstraint e2 argTypeInfo tinfo1
  addConstraint e2 argTypeInfo tinfo2
  return $ TypeInfo TBool torig (located e2)

tcEqual :: BinOp -> Loc -> TypeOrigin -> LExpr -> LExpr -> InferM TypeInfo
tcEqual op opLoc torig e1 e2 = do
  tinfo1 <- tcLExpr e1
  tinfo2 <- tcLExpr e2
  case (ttype tinfo1, ttype tinfo2) of
    (TContract, _) -> addConstrAndRetBool (tContractInfo torig opLoc, tinfo2)
    (TAccount, _)  -> addConstrAndRetBool (tAccountInfo torig opLoc, tinfo2)
    (TBool, _)     -> addConstrAndRetBool (tBoolInfo torig opLoc, tinfo2)
    (TAsset at, _) -> addConstrAndRetBool (tAssetInfo at torig opLoc, tinfo2)
    (TDateTime, _) -> addConstrAndRetBool (tDatetimeInfo torig opLoc, tinfo2)
    (TTimeDelta, _) -> addConstrAndRetBool (tDeltaInfo torig opLoc, tinfo2)
    (TText, _)      -> addConstrAndRetBool (tMsgInfo torig opLoc, tinfo2)
    (TVar _, _)     -> addConstrAndRetBool (tinfo1, tinfo2)
    (_, TVar _)     -> addConstrAndRetBool (tinfo1, tinfo2)
    (TNum _, TNum _) -> return $ tBoolInfo torig eLoc
    (t1,t2)        -> do
      throwErrInferM (InvalidBinOp op t1 t2) opLoc
      return $ tBoolInfo torig eLoc
  where
    eLoc = located e2
    addConstrAndRetBool = addConstrAndRetInfo e2 $ tBoolInfo torig eLoc

tcLEqual :: BinOp -> Loc -> TypeOrigin -> LExpr -> LExpr -> InferM TypeInfo
tcLEqual op opLoc torig e1 e2 = do
  tinfo1 <- tcLExpr e1
  tinfo2 <- tcLExpr e2
  case (ttype tinfo1, ttype tinfo2) of
    (TDateTime, _)   -> addConstrAndRetBool (tDatetimeInfo torig opLoc, tinfo2)
    (TTimeDelta, _)  -> addConstrAndRetBool (tDeltaInfo torig opLoc, tinfo2)
    (TVar _, _)      -> addConstrAndRetBool (tinfo1, tinfo2)
    (_, TVar _)      -> addConstrAndRetBool (tinfo1, tinfo2)
    (TNum _, _)      -> addConstrAndRetBool (tinfo1, tinfo2)
    (t1,t2)          -> do
      throwErrInferM (InvalidBinOp op t1 t2) opLoc
      return $ TypeInfo TError torig eLoc
  where
    eLoc = located e2
    addConstrAndRetBool = addConstrAndRetInfo e2 $ tBoolInfo torig eLoc

tcGEqual :: Loc -> TypeOrigin -> LExpr -> LExpr -> InferM TypeInfo
tcGEqual = tcLEqual GEqual

tcGreater :: Loc -> TypeOrigin -> LExpr -> LExpr -> InferM TypeInfo
tcGreater = tcLEqual Greater

tcLesser :: Loc -> TypeOrigin -> LExpr -> LExpr -> InferM TypeInfo
tcLesser = tcLEqual Lesser

tcNEqual :: Loc -> TypeOrigin -> LExpr -> LExpr -> InferM TypeInfo
tcNEqual = tcEqual NEqual

tcNotOp :: Loc -> TypeOrigin -> LExpr -> InferM TypeInfo
tcNotOp opLoc torig e1 = do
  tinfo <- tcLExpr e1
  case ttype tinfo of
    TBool       -> return $ tBoolInfo torig eLoc
    t@TNum{}    -> pure $ TypeInfo t torig eLoc
    invalidUnOp -> do
      throwErrInferM (InvalidUnOp Not invalidUnOp) opLoc
      return $ TypeInfo TError torig eLoc
  where
    eLoc = located e1

tcRecordAccess :: Loc -> unused -> LExpr -> LExpr -> InferM TypeInfo
tcRecordAccess loc _ e1 e2 = do
    tyInfoE1 <- tcLExpr e1
    case ttype tyInfoE1 of
      TADT tyName -> case e2 of
        (Located _ (EVar (Located _ field))) ->
          Map.lookup tyName <$> asks adtToConstrsAndFields >>= \case
            Just (_, validFields) -> do
              case List.lookup field validFields of
                Nothing -> throwErrInferM InvalidField{ tyErrField = field, tyErrTyName = tyName, validFields } loc
                Just fieldTy -> pure $ TypeInfo fieldTy (FromRecordAccess field tyName) loc
            Nothing -> panic $ "tcRecordAccess: don't have adt info for " <> prettyPrint tyName
        _ -> throwErrInferM ExpectedField{ tyErrTyName = tyName } loc
      _ -> throwErrInferM (FieldAccessOnNonADT tyInfoE1) loc

-- | Helper for common pattern "Add constraint of two TypeInfos and return a TypeInfo"
addConstrAndRetInfo :: LExpr -> TypeInfo -> (TypeInfo, TypeInfo) -> InferM TypeInfo
addConstrAndRetInfo le retInfo (expected, actual) =
  addConstraint le expected actual >> return retInfo


--------------------------------------------------------------------------------
-- Checking method preconditions
--------------------------------------------------------------------------------

tcPreconditions :: Preconditions -> InferM ()
tcPreconditions (Preconditions ps) = mapM_ (tcPrecondition) ps
  where
    tcPrecondition :: (Precondition, LExpr) -> InferM ()
    tcPrecondition (p,e) = addConstraint e tyExpected =<< tcLExpr e
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

data Constraint = Constraint
  { mOrigLExpr :: Maybe LExpr -- ^ Maybe the expression from which the constraint originated
  , expected :: TypeInfo
  , actual   :: TypeInfo
  } deriving (Show)

-- | Add a constraint during the constraint generation phase
addConstraint :: LExpr -> TypeInfo -> TypeInfo -> InferM ()
addConstraint lexpr expected' actual' = modify' $ \s ->
  s { constraints = constraints s ++ [c] }
  where
    c = Constraint
      { mOrigLExpr = Just lexpr
      , expected = expected'
      , actual   = actual'
      }

addConstraint' :: TypeInfo -> TypeInfo -> InferM ()
addConstraint' expected actual = modify' $ \s ->
  s { constraints = constraints s ++ [c] }
  where
    c = Constraint
      { mOrigLExpr = Nothing
      , expected
      , actual
      }

instance Pretty Constraint where
  ppr (Constraint mLExpr ti1 ti2) =
    "Constraint:"
    <$$+> (maybe "" (ppr . locVal) mLExpr)
    <$$+> ppr ti1
    <$$+> ppr ti2

instance Pretty [Constraint] where
  ppr [] = ""
  ppr (c:cs) = ppr c <$$> Language.FCL.Pretty.line <> ppr cs

-------------------------------------------------------------------------------
-- Inference Utils
-------------------------------------------------------------------------------

typeVars :: [Text]
typeVars = [1..] >>= flip replicateM ['a'..'z'] >>= return . toS

freshText :: InferM Text
freshText = do
  inferState <- get
  put $ inferState { count = count inferState + 1 }
  return $ (typeVars `unsafeIndex` count inferState)

freshTVar :: InferM Type
freshTVar = TVar <$> freshTVar'

freshTVar' :: InferM TVar
freshTVar' = TV <$> freshText

freshTAVar :: InferM Type
freshTAVar = TVar <$> freshTAVar'

freshTAVar' :: InferM TVar
freshTAVar' = TAV <$> freshText

freshTCVar :: InferM Type
freshTCVar = TVar <$> freshTCVar'

freshTCVar' :: InferM TVar
freshTCVar' = TCV <$> freshText

lookupVarType :: LName -> InferM (Maybe (TMeta, TypeInfo))
lookupVarType (Located loc name) = lookupContext name <$> gets context

-- | Just like 'lookupVarType' but throws a type error if the variable doesn't
-- exist in the typing env.
lookupVarType' :: LName -> InferM (TMeta, TypeInfo)
lookupVarType' var@(Located loc name) = do
  mVarTypeInfo <- lookupVarType var
  case mVarTypeInfo of
    Nothing -> (Temp,) <$> throwErrInferM (UnboundVariable name) loc
    Just typeInfo -> return typeInfo

-- | Look up the type of the lhs of an assignment, which could be a record
-- field.
--
lookupAssignVarType
  :: LName -- ^ the variable we are assigning to
  -> [Name] -- ^ potential fields of the variable we are assigning to
  -> InferM (Maybe (TMeta, TypeInfo))

-- If there are no fields, we just do a normal variable lookup.
lookupAssignVarType v [] = lookupVarType v

-- If there are fields, we first get the type of the variable. If the variable doesn't exist then
-- this is an error because it can't be a new temporary variable. We then check recursively if all
-- the field accesses are valid and welltyped.
lookupAssignVarType v (fd : fds) = lookupVarType v >>= \case
    Nothing -> Just . (Temp,) <$> throwErrInferM (UnboundVariable (locVal v)) loc
    Just (meta, tyInfo@(TypeInfo (TADT tyNm) _ _)) -> checkFields tyNm fd fds
      where
        checkFields :: Name -> Name -> [Name] -> InferM (Maybe (TMeta, TypeInfo))
        checkFields tyName fd fds = do
            (_, validFields) <- (Map.! tyName) . adtToConstrsAndFields <$> ask
            case (List.lookup fd validFields, fds) of
              (Nothing, _) ->
                Just . (meta,) <$> throwErrInferM InvalidField{ tyErrField = fd, tyErrTyName = tyName, validFields} loc
              (Just fieldTy, []) -> pure $ Just (meta, TypeInfo fieldTy (FromRecordAccess fd tyName) loc)
              (Just (TADT tyName), (fd:fds)) -> checkFields tyName fd fds
              (Just ty, _) -> Just . (meta,) <$> throwErrInferM (FieldAccessOnNonADT tyInfo) loc

    Just (meta, tyInfo)
      -> Just . (meta,) <$> throwErrInferM (FieldAccessOnNonADT tyInfo) loc
  where
    loc = located v

-- | Checks if # args supplied to function match # args in Sig,
-- returns a boolean indicating whether this is true or not.
arityCheck :: LName -> [Type] -> [LExpr] -> InferM Bool
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
  apply _ t@TNum{}    = t
  apply s TBool       = TBool
  apply s TAccount    = TAccount
  apply s (TAsset at) = TAsset at
  apply s TContract   = TContract
  apply s TText       = TText
  apply s TVoid       = TVoid
  apply s TSig        = TSig
  apply s TAny        = TAny
  apply s TState      = TState
  apply s TDateTime   = TDateTime
  apply s (TFun ats rt) = TFun (map (apply s) ats) (apply s rt)
  apply s t@TADT{}   = t
  apply s TTimeDelta  = TTimeDelta
  apply s (TColl tc)  = TColl (apply s tc)
  apply s TTransition = TTransition

instance Substitutable TCollection where
  apply s (TMap k v) = TMap (apply s k) (apply s v)
  apply s (TSet v)   = TSet (apply s v)

instance (Substitutable a, Substitutable b) => Substitutable (a,b) where
  apply s (a,b) = (apply s a, apply s b)

instance Substitutable TypeInfo where
  apply s (TypeInfo t info tPos) = TypeInfo (apply s t) info tPos

instance Substitutable TMeta where
  apply s tmeta = tmeta

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply

instance Substitutable Constraint where
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

data Unifier = Unifier Subst [Constraint]

instance Semigroup Unifier where
  (Unifier subst cs) <> (Unifier subst' cs') =
    Unifier (subst' `composeSubst` subst) (cs' ++ apply subst' cs)

instance Monoid Unifier where
  mempty = Unifier mempty mempty

instance {-# OVERLAPS #-} Semigroup (Either a Unifier) where
  -- Note: Short circuits on Left, does not accumulate values.
  (Left e) <> _           = Left e
  _ <> (Left e)          = Left e
  (Right u) <> (Right u') = Right (u <> u')

instance {-# OVERLAPS #-} Monoid (Either a Unifier) where
  mempty = Right mempty

instance Pretty Unifier where
  ppr (Unifier subst cs) = "Unifier:"
               <$$+> ("Subst:" <$$+> ppr subst)
               <$$+> ("Constraints:" <$$+> ppr cs)

type SolverM = State [TypeError]

runSolverM :: InferState -> Either (NonEmpty TypeError) Subst
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
    mkHoleError :: Hole -> TypeError
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

throwErrSolverM :: TypeError -> SolverM Unifier
throwErrSolverM typeErr = do
  modify' $ flip (++) [typeErr]
  return mempty

bind ::  TVar -> Type -> Unifier
bind tv t = Unifier (Subst (Map.singleton tv t)) []

unify :: Constraint -> SolverM Unifier
unify (Constraint mLExpr to1 to2) =
    case unify' t1 t2 of
      Left tErrInfo -> throwErrSolverM $ TypeError tErrInfo terrLoc
      Right unifier -> return unifier
  where
    TypeInfo t1 t1orig t1Pos = to1
    TypeInfo t2 t2orig t2Pos = to2

    terrLoc = maybe t2Pos located mLExpr

    unificationFail = Left $ UnificationFail to1 to2

    unify' :: Type -> Type -> Either TypeErrInfo Unifier
    unify' t1 t2 | t1 == t2          = Right mempty
    unify' TAny   _                  = Right mempty
    unify' _      TAny               = Right mempty
    unify' (TVar v) t                = unifyTVar t v
    unify' t (TVar v)                = unifyTVar t v
    unify' (TAsset TAny) (TAsset ta) = Right mempty
    unify' (TAsset ta) (TAsset TAny) = Right mempty
    unify' TError t                  = Right mempty
    unify' t TError                  = Right mempty
    unify' (TColl tc1) (TColl tc2)   = unifyTColl tc1 tc2
    unify' (TFun as r) (TFun as' r') = unifyTFun (as,r) (as',r')
    unify' (TNum p1) (TNum p2)       = unifyNumPrecision p1 p2
    unify' t1 t2                     = unificationFail

    -- Generate unifiers for all arguments/return types for both functions. Here
    -- we do not generate constraints as per usual unification of functions for
    -- a bit more explicit type error (reporting that the function types don't
    -- unify, rather than the individual arg/ret type pairs don't unify).
    unifyTFun :: ([Type], Type) -> ([Type], Type) -> Either TypeErrInfo Unifier
    unifyTFun (as,r) (as',r')
      | length as == length as' =
          foldMap (uncurry unify') (zip (as ++ [r]) (as' ++ [r']))
        -- Right (mempty, constraints)
      | otherwise = unificationFail -- Fail because of arity mismatch

    unifyTColl :: TCollection -> TCollection -> Either TypeErrInfo Unifier
    unifyTColl (TMap k1 v1) (TMap k2 v2) = unify' k1 k2 >> unify' v1 v2
    unifyTColl (TSet v1)    (TSet v2)    = unify' v1 v2
    unifyTColl _            _            = unificationFail

    unifyTVar :: Type -> TVar -> Either TypeErrInfo Unifier
    -- TV (general type vars) unify with anything
    unifyTVar t            v@(TV _)  = Right $ v `bind` t
    unifyTVar t@(TVar (TV _)) v      = Right $ v `bind` t

   -- TAV (asset specific type vars) unify with only 3 types
    unifyTVar t@(TNum p1)  v@(THV (TAsset (TNum p2)))
      = unifyNumPrecision p2 p1 <> Right (v `bind` t) -- the holdings can be more precise
    unifyTVar TBool        v@(THV (TAsset TBool)) = Right $ v `bind` TBool
    unifyTVar t@(TVar (THV _)) v@(THV _) = Right $ v `bind` t -- THVar binds with THVar
    unifyTVar _            v@(THV _) = unificationFail

    unifyTVar (TAsset TAny) v@(TAV _) = Right mempty
    unifyTVar t@(TAsset ta) v@(TAV tavar) = Right $ v `bind` t
    unifyTVar t@(TVar (TAV _)) v@(TAV _) = Right $ v `bind` t -- TAVar binds with TAVar
    unifyTVar _             v@(TAV _) = unificationFail

    -- TCV (collection specific type vars)
    unifyTVar t@(TColl _)      v@(TCV c) = Right $ v `bind` t
    unifyTVar t@(TVar (TCV _)) v@(TCV c) = Right $ v `bind` t -- TCVar binds with TCVar
    unifyTVar _                v@(TCV _) = unificationFail

    unifyNumPrecision :: NumPrecision -> NumPrecision -> Either TypeErrInfo Unifier
    unifyNumPrecision p1 p2 = case (normaliseNumPrecision p1, normaliseNumPrecision p2) of
        (NPDecimalPlaces i1, NPDecimalPlaces i2) | i1 >= i2 -> pure mempty
        (NPArbitrary, _) -> pure mempty
        (p1, p2) -> Left $ NumPrecisionMismatch to1 to2

solver :: Unifier -> SolverM Subst
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

instance Pretty TypeOrigin where
  ppr torig = case torig of
    OutOfThinAir        -> "is a fresh type variable"
    ExpectedFromSeq -> "is the expected type of the statement"
    ExpectedFromMethodBody -> "is the expected type of the method body"
    VariableDefn nm     -> "inferred by top level definition for variable" <+> sqppr nm
    InferredFromVar nm  -> "inferred variable" <+> sqppr nm
    InferredFromLit lit -> "inferred from literal" <+> sqppr lit
    InferredFromExpr e  -> "inferred from expression" <+> sqppr e
    InferredFromAssetType nm t -> "inferred from the asset type" <+> sqppr t <+> "supplied as an argument to the primop" <+> sqppr nm
    InferredFromHelperDef nm -> "inferred from the helper function definition" <+> sqppr nm
    InferredFromAssignment nms -> "inferred from the assignment to variable" <+> (sqppr . mconcat . intersperse "." . map ppr . toList) nms
    InferredFromMethodBody -> "inferred from the body of method"
    BinaryOperator op   -> "inferred from use of binary operator" <+> sqppr op
    UnaryOperator op    -> "inferred from use of unary operator" <+> sqppr op
    Assignment          -> "inferred from variable assignment"
    IfCondition         -> "inferred from the condition of an if statement"
    DateTimeGuardPred   -> "must be a datetime because it is a datetime guard predicate"
    DateTimeGuardBody   -> "must be a void because it is the body of a datetime guard"
    FunctionArg n nm    -> "inferred from the type signature of argument" <+> ppr n <+> "of function" <+> sqppr nm
    FunctionRet nm      -> "inferred from the return type of the function" <+> sqppr nm
    FromConstructor ty -> "inferred from type of constructor" <+> sqppr ty
    FromConstructorField c paramName
      -> "inferred from type of field" <+> sqppr paramName <+> "of constructor" <+> sqppr c
    FromCase loc -> "inferred from" <+> sqppr Token.case_ <+> "at" <+> ppr loc
    CaseBody e          -> "inferred from type of case body" <+> sqppr e
    FromVariablePattern v -> "inferred from variable binding" <+> sqppr v <+> "in case body"
    InferredFromCollType nm t -> "inferred from the collection type" <+> sqppr t <+> "supplied as an argument to the primop" <+> sqppr nm
    EmptyCollection     -> "inferred from an empty collection"
    MapExpr             -> "inferred from a map expression"
    SetExpr             -> "inferred from a set expression"
    FromPrecondition p -> "inferred from a precondition" <+> sqppr p
    FromHole            -> "coming from a hole in the program"
    EmptyBlock          -> "inferred from an empty block (e.g. if-statement without else-branch)"
    FromRoundingPrecision -> "inferred from the precision of a rounding operation"
    FromRecordAccess fd tyNm -> "inferred from accessing" <+> sqppr fd <+> "which is a field of" <+> sqppr tyNm

instance Pretty TypeInfo where
  ppr (TypeInfo t orig loc) = sqppr t <+> ppr orig <+> "on" <+> ppr loc

instance Pretty TypeErrInfo where
  ppr = \case
    UnboundVariable nm            -> "Unbound variable: " <+> ppr nm
    Shadow id meta (TypeInfo t _ loc)
      -> "The binding" <+> sqppr id <+> "shadows the" <+> ppr meta
      <$$+> "of the same name, of type" <+> sqppr t <> ","
      <$$+> "bound at" <+> ppr loc <> "."
    InvalidDefinition nm e lhsTyp rhsTyp
                                  -> "Invalid definition for" <+> ppr nm <> ":"
                                  <$$+> "Expected type:" <+> ppr lhsTyp
                                  <$$+> "But inferred type:" <+> ppr rhsTyp <+> "for expression" <+> sqppr e
    UndefinedFunction nm          -> "Invalid function name: " <+> ppr nm
    InvalidBinOp op t1 t2         -> "Invalid binary operation: "
                                  <$$+> sqppr op <+> "does not accept types" <+> ppr t1 <+> "and" <+> ppr t2
    InvalidUnOp op t              -> "Invalid unary operation: "
                                  <$$+> sqppr op <+> "does not accept types" <+> ppr t
    InvalidAddress nm             -> "Invalid address: " <+> ppr nm
                                  <$$+> "Addresses must be a valid base 58 encoded sha256 hashes."
    ArityFail nm n m              -> "Arity mismatch in function call" <+> ppr nm <> ":"
                                  <$$+> "Expecting" <+> ppr n <+> "arguments, but got" <+> ppr m
    InvalidArgType nm t1 t2       -> "Invalid argument type to method" <+> ppr nm <> ":"
                                  <$$+> "Expecting type" <+> ppr t1 <+> "but got" <+> ppr t2
    VarNotFunction nm t           -> "Variable" <+> sqppr nm <+> "is not a helper function"
                                  <$$+> "Expecting a function type, but got" <+> sqppr t
    UnificationFail tinfo1 tinfo2 -> "Expected:" <+> ppr tinfo1
                                <$$> "But got: " <+> ppr tinfo2
    TransitionOnlyInTrueBranch cond
      -> "A transition occurs when the condition" <+> sqppr cond
        <+> "of the if-statement is true,"
        <$$+> "but there is no else-branch that specifies where to transition to when the condition is false."
    TransitionOnlyInOneBranch
      -> "A transition occurs in one branch of the if-statement, but not in the other."
    UnreachableStatement loc
      -> "This statement transitions, leaving the following statement at"
        <+> ppr loc <+> "unreachable."
    ExpectedStatement ty
      -> "Expected a statement but got something of type" <+> sqppr ty
    ExpectedExpressionAssRHS stmt
      -> "Expected an expression on the right hand side of an assignment but got the statement" <+> sqppr stmt
    MethodUnspecifiedTransition name
      -> "Method" <+> sqppr name <+> "does not specify where to transition to."
        <$$+> "If the method should not transition, use the" <+> squotes (ppr Stay <> "()")
        <+> "primop."
    CaseOnNotADT e               -> "Case analysis on a non-adt type:"
                                  <$$+> ppr e
    UnknownConstructor c          -> "Reference to undefined constructor:"
                                  <$$+> ppr c
    UnknownADT e                 -> "Reference to undefined type:"
                                  <$$+> ppr e
    PatternMatchError misses dups -> "Pattern match failures:"
                                  <$$+> vsep (map ((" - Missing case for:" <+>) . ppr) misses)
                                  <$$+> vsep (map ((" - Duplicate case for:" <+>) . ppr) dups)
    TooManyPatterns c p           -> "Constructor" <+> sqppr c <+> "is applied to too many patterns (" <> sqppr p <> ")"
    TooManyArguments c e          -> "Constructor" <+> sqppr c <+> "is applied to too many arguments (" <> sqppr e <> ")"
    NotEnoughArguments c (n, t)   -> "Constructor" <+> sqppr c <+> "is not applied to enough arguments; expecting an argument of type" <+> sqppr t <+> "for field" <+> sqppr n
    EmptyMatches                  -> "Case expression with no matches"
    InvalidPrecision              -> "Invalid precision argument; must be an integer (whole number) literal."
    Impossible msg                -> "The impossible happened:" <+> ppr msg
    UnknownHoleInProgram          -> "Expecting an expression or statement here."
    HoleInProgram{ inferredType, suggestions }
      -> "Expecting something of type" <+> sqppr inferredType <> "."
      <$$+> (notNull suggestions ? "Suggestions:"
            <$$+> vsep (map pprSuggestion suggestions))
        where
          pprSuggestion (name, (tmeta, _)) = ppr tmeta <+> ppr name
          notNull = not . null
    NumPrecisionMismatch typeInfo1 typeInfo2 ->
        ("Implicit loss of numeric precision"
            <$$+> (ppr typeInfo1
              <$$> ppr typeInfo2))
        <$$> "Possible fixes:"
         <$$+> ( vcat $ map ("•" <+>)
            [ "round the higher precision expression to match the lower precision"
            , "use types with more (e.g. arbitrary) precision"])
    FieldTypeMismatch fields
      -> ("Conflicting fields in a record declaration:"
          <$$+> vcat (toList $ map (\(Located loc fd, ty) -> sqppr (ppr ty <+> ppr fd) <+> "at" <+> ppr loc) fields))
          <$$> "Record fields of the same name must have the same type."
    FieldAccessOnNonADT badType
      -> "Expecting a record type on the left hand side of a record access but got" <+> sqppr badType <> "."
    InvalidField{ tyErrField, tyErrTyName, validFields = [] }
      -> "Reference to invalid field" <+> sqppr tyErrField <> ". The data type" <+> sqppr tyErrTyName
        <+> "does not have any valid fields. Fields are only valid if they are defined for every constructor."
    InvalidField{ tyErrField, tyErrTyName, validFields }
      -> "Reference to invalid field" <+> sqppr tyErrField <> ". The data type" <+> sqppr tyErrTyName
        <+> "has the following fields defined for every constructor:"
        <$$+> vcat (map (\(fd, ty) -> ppr ty <+> ppr fd) validFields)
    ExpectedField tyName
      -> "Expected a field name but got an expression in the record access for type" <+> sqppr tyName

instance Pretty TypeError where
  ppr (TypeError tErrInfo tPos) = errName <+> "at" <+> ppr tPos <> ":"
                                  <$$+> ppr tErrInfo
    where
      errName = case tErrInfo of
        HoleInProgram{} -> "Hole"
        UnknownHoleInProgram{} -> "Hole"
        UnificationFail{} -> "Type error"
        TransitionOnlyInTrueBranch{} -> "Transition error"
        TransitionOnlyInOneBranch{} -> "Transition error"
        UnreachableStatement{} -> "Transition error"
        _ -> "Error"

instance Pretty [TypeError] where
  ppr es = case map ppr (sort es) of
    [] -> ""
    (e:es) -> foldl' (<$$$>) (ppr e) $ map ppr es

instance Pretty (NonEmpty TypeError) where
  ppr = ppr . NonEmpty.toList

-- | Pretty print a type error
ppError :: TypeError -> LText
ppError = render . ppr

-- | Pretty print a type signature
ppSig :: Sig -> LText
ppSig = render . ppr
