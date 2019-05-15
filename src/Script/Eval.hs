{-|

FCL interpreter and expression evaluation.

-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Script.Eval (
  -- ** Evaluation monad
  EvalM,
  EvalFail(..),
  runEvalM,
  execEvalM,

  -- ** Evaluation rules
  eval,
  evalLLit,
  evalLExpr,
  evalMethod,
  evalCallableMethods,

  -- ** Evaluation state
  EvalState(..),
  initEvalState,

  -- ** Evaluation context
  TransactionCtx(..),
  EvalCtx(..),
) where

import Protolude hiding (DivideByZero, Overflow, Underflow, StateT, execStateT, runStateT, modify, get, gets)

import Numeric.Lossless.Number
import Script
import SafeInteger
import SafeString as SS
import Key (PrivateKey)
import Time (Timestamp, posixMicroSecsToDatetime)
import Ledger
import Storage
import Script.Error as Error
import Script.Prim (PrimOp(..))
import Script.ReachabilityGraph (applyTransition)
import Utils (panicImpossible)
import qualified Asset
import qualified Delta
import qualified Contract
import qualified Hash
import qualified Key
import qualified Ledger
import qualified Script.Pretty as Pretty
import qualified Script.Prim as Prim
import Utils (traverseWithKey')

import qualified Datetime as DT
import Datetime.Types (within, Interval(..), add, sub, subDeltas, scaleDelta)
import qualified Datetime.Types as DT

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Serialize as S

import qualified Control.Exception as E
import qualified Control.Monad.Catch as Catch
import Control.Monad.State.Strict
import qualified Crypto.Random as Crypto (SystemDRG, getSystemDRG)
import qualified Crypto.Random.Types as Crypto (MonadRandom(..), MonadPseudoRandom, withDRG)

import qualified Encoding

-------------------------------------------------------------------------------
-- Evaluation Context / State
-------------------------------------------------------------------------------

-- | The context of the current transaction
data TransactionCtx = TransactionCtx
  { transactionHash     :: Hash.Hash Encoding.Base16ByteString -- ^ Hash of the transaction
  , transactionIssuer   :: Addr                    -- ^ Issuer of the transaction
  , transactionBlockIdx :: Int64                               -- ^ Index of the block in which the transaction is contained
  , transactionBlockTs  :: Time.Timestamp                      -- ^ The timestamp of the block in which the transaction is contained
  } deriving (Generic)

-- | Evaluation context used during remote evaluation in a validating engine.
data EvalCtx = EvalCtx
  { currentValidator   :: Addr   -- ^ Referencing an account
  , currentTxCtx       :: Maybe TransactionCtx -- ^ Information about the current transaction
  , currentCreated     :: Time.Timestamp    -- ^ When the contract was deployed
  , currentDeployer    :: Addr  -- ^ Referencing an account
  , currentAddress     :: Addr -- ^ Address of current Contract
  , currentPrivKey     :: Key.PrivateKey    -- ^ Private key of Validating node
  , currentHelpers     :: [Helper]          -- ^ Script helper functions available for call in method body
  } deriving (Generic)

type LocalStorages = Map.Map (Addr) Storage

data EvalState = EvalState
  { tempStorage      :: Storage          -- ^ Tmp variable env
  , globalStorage    :: Storage          -- ^ Global variable env
  , workflowState    :: WorkflowState    -- ^ Current state of contract
  , currentMethod    :: Maybe Method     -- ^ Which method we're currently in, if any
  , worldState       :: World            -- ^ Current world state
  , deltas           :: [Delta.Delta]
  } deriving (Generic, Show)

{- TopLevel:
 -
 - NodeData
 -   > latestBlock index
 -   > node account address
 -   > ledger state
 -   > private key
 -   > storage key
 -
 - Transaction
 -   > current transaction hash
 -   > origin address of transaction
 -
 - Contract
 -   > timestamp
 -   > address
 -}

initEvalState :: Contract.Contract -> World -> EvalState
initEvalState c w = EvalState
  { tempStorage      = mempty
  , globalStorage    = Storage.unGlobalStorage (Contract.globalStorage c)
  , workflowState    = Contract.state c
  , currentMethod    = Nothing
  , worldState       = w
  , deltas           = []
  }

-------------------------------------------------------------------------------
-- Interpreter Steps
-------------------------------------------------------------------------------

lookupGlobalVar :: Name -> EvalM (Maybe Value)
lookupGlobalVar (Name var) = do
  globalStore <- gets globalStorage
  return $ Map.lookup (Key var) globalStore

lookupTempVar :: Name -> EvalM (Maybe Value)
lookupTempVar (Name var) = do
  tmpStore <- gets tempStorage
  return $ Map.lookup (Key var) tmpStore

insertTempVar :: Name -> Value -> EvalM ()
insertTempVar (Name var) val = modify' $ \evalState ->
    evalState { tempStorage = insertVar (tempStorage evalState) }
  where
    insertVar = Map.insert (Key var) val

-- | Extends the temp storage with temporary variable updates. Emulates a
-- closure environment for evaluating the body of helper functions by
-- assigning values to argument names. Effectively ad-hoc substitution.
localTempStorage :: [(Name,Value)] -> EvalM a -> EvalM a
localTempStorage varVals evalM = do
  currTempStorage <- tempStorage <$> get
  let store = Map.fromList (map (first (Key . unName)) varVals)
      newTempStorage = Map.union store currTempStorage
  modify $ \evalState ->
    evalState { tempStorage = newTempStorage }
  res <- evalM
  modify $ \evalState ->
    evalState { tempStorage = currTempStorage }
  pure res

-- | Warning: This function will throw an exception on a non-existent helper, as
-- this indicates the typechecker failed to spot an undefined function name.
lookupHelper :: LName -> EvalM Helper
lookupHelper lhnm = do
  helpers <- currentHelpers <$> ask
  case List.find ((==) lhnm . helperName) helpers of
    Nothing     -> panicImpossible "lookupHelper: Undefined helper function name"
    Just helper -> pure helper

-- | Emit a delta updating  the state of a global reference.
updateGlobalVar :: Name -> Value -> EvalM ()
updateGlobalVar (Name var) val = modify' $ \evalState ->
    evalState { globalStorage = updateVar (globalStorage evalState) }
  where
    updateVar = Map.update (\_ -> Just val) (Key var)

setWorld :: World -> EvalM ()
setWorld w = modify' $ \evalState -> evalState { worldState = w }

-- | Update the evaluate state.
updateState :: WorkflowState -> EvalM ()
updateState newState = modify' $ \s -> s { workflowState = newState }

-- | Get the evaluation state
getState :: EvalM WorkflowState
getState = gets workflowState

setCurrentMethod :: Method -> EvalM ()
setCurrentMethod m = modify' $ \s -> s { currentMethod = Just m }

-- | Emit a delta
emitDelta :: Delta.Delta -> EvalM ()
emitDelta delta = modify' $ \s -> s { deltas = deltas s ++ [delta] }

-- | Lookup variable in scope
lookupVar :: Name -> EvalM (Maybe Value)
lookupVar var = do
  gVar <- lookupGlobalVar var
  case gVar of
    Nothing  -> lookupTempVar var
    Just val -> return $ Just val

transactionCtxField :: Loc -> Text -> (TransactionCtx -> a) -> EvalM a
transactionCtxField loc errMsg getField = do
  mfield <- fmap getField . currentTxCtx <$> ask
  case mfield of
    Nothing -> throwError $ NoTransactionContext loc errMsg
    Just field -> pure field

currBlockTimestamp :: Loc -> EvalM Time.Timestamp
currBlockTimestamp loc = do
  let errMsg = "Cannot get timestamp without a transaction context"
  transactionCtxField loc errMsg transactionBlockTs

currentBlockIdx :: Loc -> EvalM Int64
currentBlockIdx loc = do
  let errMsg = "Cannot get block index without a transaction context"
  transactionCtxField loc errMsg transactionBlockIdx

currentTxHash :: Loc -> EvalM (Hash.Hash Encoding.Base16ByteString)
currentTxHash loc = do
  let errMsg = "Cannot get current transaction hash without a transaction context"
  transactionCtxField loc errMsg transactionHash

currentTxIssuer :: Loc -> EvalM Addr
currentTxIssuer loc = do
  let errMsg = "Cannot get current transaction issuer without a transaction context"
  transactionCtxField loc errMsg transactionIssuer

-------------------------------------------------------------------------------
-- Interpreter Monad
-------------------------------------------------------------------------------

type RandomM = Crypto.MonadPseudoRandom Crypto.SystemDRG

-- | Initialize the random number generator and run the monadic
-- action.
runRandom :: RandomM a -> IO a
runRandom m = do
  gen <- Crypto.getSystemDRG
  return . fst . Crypto.withDRG gen $ m

-- | EvalM monad
type EvalM = ReaderT EvalCtx (StateT EvalState (ExceptT Error.EvalFail RandomM))

instance Crypto.MonadRandom EvalM where
  getRandomBytes = lift . lift . lift . Crypto.getRandomBytes

-- | Run the evaluation monad.
execEvalM :: EvalCtx -> EvalState -> EvalM a -> IO (Either Error.EvalFail EvalState)
execEvalM evalCtx evalState
  = handleArithError
  . runRandom
  . runExceptT
  . flip execStateT evalState
  . flip runReaderT evalCtx

-- | Run the evaluation monad.
runEvalM :: EvalCtx -> EvalState -> EvalM a -> IO (Either Error.EvalFail (a, EvalState))
runEvalM evalCtx evalState
  = handleArithError
  . runRandom
  . runExceptT
  . flip runStateT evalState
  . flip runReaderT evalCtx

handleArithError :: IO (Either Error.EvalFail a) -> IO (Either Error.EvalFail a)
handleArithError m = do
   res <- Catch.try $! m
   case res of
    Left E.Overflow              -> return . Left $ Overflow
    Left E.Underflow             -> return . Left $ Underflow
    Left (e :: E.ArithException) -> return . Left $ Impossible "Arithmetic exception"
    Right val                    -> pure val

-------------------------------------------------------------------------------
-- Evaluation
-------------------------------------------------------------------------------

-- | Evaluator for expressions
evalLExpr :: LExpr -> EvalM Value
evalLExpr (Located loc e) = case e of

  ESeq a b        -> evalLExpr a >> evalLExpr b

  ELit llit       -> pure $ evalLLit llit

  EAssign lhs rhs -> do
    gVal <- lookupGlobalVar lhs
    case gVal of
      Nothing -> do
        v <- evalLExpr rhs
        insertTempVar lhs v
      Just initVal -> do
        resVal <- evalLExpr rhs
        when (initVal /= resVal) $ do
          updateGlobalVar lhs resVal
          emitDelta $ Delta.ModifyGlobal lhs resVal
    pure VVoid

  EUnOp (Located _ op) a -> do
    valA <- evalLExpr a
    let unOpFail = panicInvalidUnOp op valA
    case valA of
      VBool a' -> return $
        case op of
          Script.Not -> VBool $ not a'
      _ -> panicImpossible "EUnOp"

  -- This logic handles the special cases of operating over homomorphic
  -- crypto-text.
  EBinOp (Located _ op) a b -> do
    valA <- evalLExpr a
    valB <- evalLExpr b
    let binOpFail = panicInvalidBinOp op valA valB
    case (valA, valB) of
      (VNum a', VNum b') ->
        case op of
          Script.Add     -> pure $ VNum (a' + b')
          Script.Sub     -> pure $ VNum (a' - b')
          Script.Mul     -> pure $ VNum (a' * b')
          Script.Div
            | b' == 0    -> throwError DivideByZero
            | otherwise  -> pure $ VNum (a' / b')
          Script.Equal   -> pure $ VBool $ a' == b'
          Script.NEqual  -> pure $ VBool $ a' /= b'
          Script.LEqual  -> pure $ VBool $ a' <= b'
          Script.GEqual  -> pure $ VBool $ a' >= b'
          Script.Lesser  -> pure $ VBool $ a' < b'
          Script.Greater -> pure $ VBool $ a' > b'
          _ -> binOpFail
      (VDateTime (DateTime dt), VTimeDelta (TimeDelta d)) ->
        case op of
          Script.Add -> pure $ VDateTime $ DateTime $ add dt d
          Script.Sub -> pure $ VDateTime $ DateTime $ sub dt d
          _ -> binOpFail
      (VTimeDelta (TimeDelta d), VDateTime (DateTime dt)) ->
        case op of
          Script.Add -> pure $ VDateTime $ DateTime $ add dt d
          Script.Sub -> pure $ VDateTime $ DateTime $ sub dt d
          _ -> binOpFail
      (VTimeDelta (TimeDelta d1), VTimeDelta (TimeDelta d2)) ->
        case op of
          Script.Add -> pure $ VTimeDelta $ TimeDelta $ d1 <> d2
          Script.Sub -> pure $ VTimeDelta $ TimeDelta $ subDeltas d1 d2
          _ -> binOpFail
      (VTimeDelta (TimeDelta d), VNum (NumDecimal (Decimal 0 n))) ->
        case op of
          Script.Mul ->
            case scaleDelta (fromIntegral n) d of
              Nothing -> binOpFail -- XXX More descriptive error
              Just newDelta -> pure $ VTimeDelta $ TimeDelta newDelta
          _ -> binOpFail
      (VBool a', VBool b') -> return $
        case op of
          Script.And -> VBool (a' && b')
          Script.Or  -> VBool (a' || b')
          Script.Equal -> VBool $ a' == b'
          Script.NEqual -> VBool $ a' /= b'
          _ -> binOpFail
      (VAccount a', VAccount b') -> return $
        case op of
          Script.Equal -> VBool $ a' == b'
          Script.NEqual -> VBool $ a' /= b'
          _ -> binOpFail
      (VAsset a', VAsset b') -> return $
        case op of
          Script.Equal -> VBool $ a' == b'
          Script.NEqual -> VBool $ a' /= b'
          _ -> binOpFail
      (VContract a', VContract b') -> return $
        case op of
          Script.Equal -> VBool $ a' == b'
          Script.NEqual -> VBool $ a' /= b'
          _ -> binOpFail
      (VDateTime a', VDateTime b') -> return $
        case op of
          Script.Equal -> VBool $ a' == b'
          Script.NEqual -> VBool $ a' /= b'
          Script.LEqual -> VBool $ a' <= b'
          Script.GEqual -> VBool $ a' >= b'
          Script.Lesser -> VBool $ a' < b'
          Script.Greater -> VBool $ a' > b'
          _ -> binOpFail
      (VText a', VText b') -> return $
        case op of
          Script.Equal -> VBool $ a' == b'
          Script.NEqual -> VBool $ a' /= b'
          Script.LEqual -> VBool $ a' <= b'
          Script.GEqual -> VBool $ a' >= b'
          Script.Lesser -> VBool $ a' < b'
          Script.Greater -> VBool $ a' > b'
          Script.Add -> VText $ a' <> b'
          _ -> binOpFail
      (v1, v2) -> panicImpossible $
        "evalLExpr EBinOp: (" <> show v1 <> ", " <> show v2 <> ")"

  EVar (Located _ var) -> do
    mVal <- lookupVar var
    case mVal of
      Nothing -> do
        globals <- gets globalStorage
        panicImpossible
          $ "evalLExpr EVar: " <> unName var <> " in " <> Pretty.prettyPrint globals
      Just val -> return val

  ECall ef args   ->
    case ef of
      Left primOp -> evalPrim loc primOp args
      Right hnm   -> do
        helper <- lookupHelper hnm
        argVals <- mapM evalLExpr args
        let argNmsAndVals =
               map (first (locVal . argName)) $
                 zip (helperArgs helper) argVals
        localTempStorage argNmsAndVals $ do
          evalLExpr (helperBody helper)

  EBefore dt e -> do
    now <- currBlockTimestamp loc
    let noLoc = Located NoLoc
    let nowDtLLit  = noLoc $ LDateTime $ DateTime $ posixMicroSecsToDatetime now
    let predicate = EBinOp (noLoc LEqual) (noLoc $ ELit nowDtLLit) dt
    evalLExpr $ noLoc $ EIf (noLoc predicate) e (noLoc ENoOp)

  EAfter dt e -> do
    now <- currBlockTimestamp loc
    let noLoc = Located NoLoc
    let nowDtLLit  = noLoc $ LDateTime $ DateTime $ posixMicroSecsToDatetime now
    let predicate = EBinOp (noLoc GEqual) (noLoc $ ELit nowDtLLit) dt
    evalLExpr $ noLoc $ EIf (noLoc predicate) e (noLoc ENoOp)

  EBetween startDte endDte e -> do
    now <- currBlockTimestamp loc
    let noLoc = Located NoLoc
    let nowDtLExpr = noLoc $ ELit $ noLoc $
          LDateTime $ DateTime $ posixMicroSecsToDatetime now
    VBool b <- evalPrim loc Between [nowDtLExpr, startDte, endDte]
    if b
      then evalLExpr e
      else noop

  EIf cond e1 e2 -> do
    VBool b <- evalLExpr cond
    if b
      then evalLExpr e1
      else evalLExpr e2

  ECase scrut ms -> do
    VEnum c <- evalLExpr scrut
    evalLExpr (match ms c)

  ENoOp -> noop

  EMap m ->
    VMap <$> traverseWithKey' (\k v -> (,) <$> evalLExpr k <*> evalLExpr v) m

  ESet s ->
    VSet . Set.fromList <$> traverse evalLExpr (toList s)

  EHole ->
    panicImpossible $ "Evaluating hole expression at " <> show loc

match :: [Match] -> EnumConstr -> LExpr
match ps c
  = fromMaybe (panicImpossible "Cannot match constructor")
  $ List.lookup (PatLit c)
  $ map (\(Match pat body) -> (locVal pat, body))
  $ ps

-- | Evaluate a binop and two Fractional Num args
evalBinOpF :: (Fractional a, Ord a) => BinOp -> (a -> Value) -> a -> a -> EvalM Value
evalBinOpF Script.Add constr a b = pure $ constr (a + b)
evalBinOpF Script.Sub constr a b = pure $ constr (a - b)
evalBinOpF Script.Mul constr a b = pure $ constr (a * b)
evalBinOpF Script.Div constr a b
  | b == 0 = throwError DivideByZero
  | otherwise = pure $ constr (a / b)
evalBinOpF Script.Equal constr a b = pure $ VBool (a == b)
evalBinOpF Script.NEqual constr a b = pure $ VBool (a /= b)
evalBinOpF Script.LEqual constr a b = pure $ VBool (a <= b)
evalBinOpF Script.GEqual constr a b = pure $ VBool (a >= b)
evalBinOpF Script.Lesser constr a b = pure $ VBool (a < b)
evalBinOpF Script.Greater constr a b = pure $ VBool (a > b)
evalBinOpF bop c a b = panicInvalidBinOp bop (c a) (c b)

evalPrim :: Loc -> PrimOp -> [LExpr] -> EvalM Value
evalPrim loc ex args = case ex of
  Now               -> do
    currDatetime <- posixMicroSecsToDatetime <$> currBlockTimestamp loc
    pure $ VDateTime $ DateTime currDatetime
  Block             -> VNum .fromIntegral <$> currentBlockIdx loc
  Deployer          -> VAccount . currentDeployer <$> ask
  Sender            -> VAccount <$> currentTxIssuer loc
  Created           -> do
    createdDatetime <- posixMicroSecsToDatetime . currentCreated <$> ask
    pure $ VDateTime $ DateTime createdDatetime
  Address           -> VContract . currentAddress <$> ask
  Validator         -> VAccount . currentValidator <$> ask

  Round -> do
    [VNum (NumDecimal (Decimal 0 p)), VNum n] <- mapM evalLExpr args
    pure . VNum . NumDecimal . roundAwayFrom0 p . toRational $ n

  RoundDown -> do
    [VNum (NumDecimal (Decimal 0 p)), VNum n] <- mapM evalLExpr args
    pure . VNum . NumDecimal . roundDown p . toRational $ n

  RoundUp -> do
    [VNum (NumDecimal (Decimal 0 p)), VNum n] <- mapM evalLExpr args
    pure . VNum . NumDecimal . roundUp p . toRational $ n

  RoundRem -> do
    [VNum (NumDecimal (Decimal 0 p)), VNum n] <- mapM evalLExpr args
    pure . VNum . NumRational . roundAwayFrom0Rem p . toRational $ n

  RoundDownRem -> do
    [VNum (NumDecimal (Decimal 0 p)), VNum n] <- mapM evalLExpr args
    pure . VNum . NumRational . roundDownRem p . toRational $ n

  RoundUpRem -> do
    [VNum (NumDecimal (Decimal 0 p)), VNum n] <- mapM evalLExpr args
    pure . VNum . NumRational . roundUpRem p . toRational $ n

  Terminate -> do
    let [Located l (LText msg)] = argLits (fmap unLoc args)
    emitDelta $ Delta.ModifyState endState
    emitDelta Delta.Terminate
    updateState endState
    pure VVoid

  TransitionTo -> do
    let [LState newLocalState] = unLoc <$> argLits (unLoc <$> args)
    m <- gets currentMethod
    case m of
      Nothing -> panicImpossible "evalPrim: transitionTo can only be called from a method body."
      Just m -> do
        oldGlobalState <- getState
        let oldLocalState = methodInputPlaces m
            t = Arrow oldLocalState newLocalState
            newGlobalState = (flip applyTransition) t oldGlobalState
        case newGlobalState of
          Just (Right newGlobalState) -> do
            emitDelta . Delta.ModifyState $ newGlobalState
            updateState newGlobalState
            pure VVoid
          Just (Left err) -> panicImpossible $
            "evalPrim: how did you manage to smuggle an unsound workflow past the soundness checker?\n"
            <> Pretty.prettyPrint err
          Nothing -> panicImpossible . Pretty.prettyPrint . Pretty.sep $
            [ "evalPrim: how did you manage to smuggle an unsound workflow past the soundness checker?"
            , Pretty.squotes (Pretty.ppr t)
            , "is not enabled in current global state"
            , Pretty.squotes (Pretty.ppr oldGlobalState)
            ]

  CurrentState -> VState <$> getState

  Stay -> do
    m <- gets currentMethod
    case m of
      Nothing -> panicImpossible "evalPrim: stay can only be called from a method body."
      Just m -> pure VVoid

  Sign           -> do
    let [msgExpr] = args
    (VText msg) <- evalLExpr msgExpr
    privKey <- currentPrivKey <$> ask -- XXX               V gen Random value?
    sig <- Key.getSignatureRS <$> Key.sign privKey (SS.toBytes msg)
    case bimap toSafeInteger toSafeInteger sig of
      (Right safeR, Right safeS) -> return $ VSig (safeR,safeS)
      otherwise -> throwError $
        HugeInteger "Signature values (r,s) too large."

  Sha256         -> do
    let [anyExpr] = args
    x <- evalLExpr anyExpr
    v <- hashValue x
    case SS.fromBytes (Hash.sha256Raw v) of
      Left err -> throwError $ HugeString $ show err
      Right msg -> return $ VText msg

  AccountExists  -> do
    let [varExpr] = args
    accAddr <- extractAddrAccount <$> evalLExpr varExpr
    world <- gets worldState
    return $ VBool $ Ledger.accountExists accAddr world

  AssetExists    -> do
    let [varExpr] = args
    assetAddr <- extractAddrAsset <$> evalLExpr varExpr
    world <- gets worldState
    return $ VBool $ Ledger.assetExists assetAddr world

  ContractExists -> do
    let [varExpr] = args
    contractAddr <- extractAddrContract <$> evalLExpr varExpr
    world <- gets worldState
    return $ VBool $ Ledger.contractExists contractAddr world

  Verify         -> do
    let [accExpr,sigExpr,msgExpr] = args
    (VSig safeSig) <- evalLExpr sigExpr
    (VText msg) <- evalLExpr msgExpr
    ledgerState <- gets worldState

    acc <- getAccount accExpr
    let sig = bimap fromSafeInteger fromSafeInteger safeSig
    return $ VBool $
      Key.verify (publicKey acc) (Key.mkSignatureRS sig) $ SS.toBytes msg

  TxHash -> do
    txHash <- currentTxHash loc
    case SS.fromBytes (Hash.getRawHash txHash) of
      Left err -> throwError $ HugeString $ show err
      Right msg -> pure $ VText msg

  ContractValue -> do
    let [contractExpr, msgExpr] = args
    contractAddr <- extractAddrContract <$> evalLExpr contractExpr
    world <- gets worldState
    case Ledger.lookupContract contractAddr world of
      Left err -> throwError $ ContractIntegrity $ show err
      Right contract -> do
        (VText varSS) <- evalLExpr msgExpr
        let var = toS $ SS.toBytes varSS
        case Contract.lookupVarGlobalStorage var contract of
          Nothing -> throwError $ ContractIntegrity $
            "Contract does not define a variable named '" <> var <> "'"
          Just val -> pure val

  ContractValueExists -> do
    -- If ContractValue throws err, value doesn't exist
    flip catchError (const $ pure $ VBool False) $ do
      _ <- evalPrim loc ContractValue args
      pure $ VBool True

  ContractState -> do
    let [contractExpr] = args
    contractAddr <- extractAddrContract <$> evalLExpr contractExpr
    world <- gets worldState
    case Ledger.lookupContract contractAddr world of
      Left err -> throwError . ContractIntegrity $ show err
      Right contract -> pure . VState $ Contract.state contract

  -- Datetime manipulation prim ops

  IsBusinessDayUK -> do
    let [dateTimeExpr] = args
    VDateTime (DateTime dt) <- evalLExpr dateTimeExpr
    return $ VBool $ DT.isBusiness DT.ukHolidays dt

  NextBusinessDayUK -> do
    let [dateTimeExpr] = args
    VDateTime (DateTime dt) <- evalLExpr dateTimeExpr
    return $ VDateTime $ DateTime $ DT.nextBusinessDay DT.ukHolidays dt

  IsBusinessDayNYSE -> do
    let [dateTimeExpr] = args
    VDateTime (DateTime dt) <- evalLExpr dateTimeExpr
    return $ VBool $ DT.isBusiness DT.nyseHolidays dt

  NextBusinessDayNYSE -> do
    let [dateTimeExpr] = args
    VDateTime (DateTime dt) <- evalLExpr dateTimeExpr
    return $ VDateTime $ DateTime $ DT.nextBusinessDay DT.nyseHolidays dt

  Between -> do
    let [dtExpr, startExpr, endExpr] = args
    VDateTime (DateTime dt) <- evalLExpr dtExpr
    VDateTime (DateTime start) <- evalLExpr startExpr
    VDateTime (DateTime end) <- evalLExpr endExpr
    return $ VBool $ within dt (Interval start end)

  TimeDiff -> do
    [VDateTime (DateTime dt), VDateTime (DateTime dt')] <- mapM evalLExpr args
    pure $ VTimeDelta (TimeDelta (DT.diff dt dt'))

  AssetPrimOp a -> evalAssetPrim loc a args
  MapPrimOp m   -> evalMapPrim m args
  SetPrimOp m   -> evalSetPrim m args
  CollPrimOp c  -> evalCollPrim c args

evalAssetPrim :: Loc -> Prim.AssetPrimOp -> [LExpr] -> EvalM Value
evalAssetPrim loc assetPrimOp args =
  case assetPrimOp of

    Prim.HolderBalance -> do
      let [assetExpr, accExpr] = args
      asset <- getAsset assetExpr
      accAddr <- getAccountAddr accExpr
      case Asset.assetType asset of
        Asset.Discrete  ->
          case Asset.balance asset (Asset.Holder accAddr) of
            Nothing  -> return $ VNum 0
            Just bal -> return . VNum . NumDecimal $ bal

        Asset.Fractional n ->
          case Asset.balance asset (Asset.Holder accAddr) of
            Nothing  -> return $ VNum 0
            Just bal -> return . VNum . NumDecimal $ bal

        Asset.Binary ->
          case Asset.balance asset (Asset.Holder accAddr) of
            Nothing  -> return $ VBool False
            Just 0 -> return $ VBool False
            Just 1 -> return $ VBool True
            Just x -> panic $ "Internal error in uplink. Expecting a boolean balance but got " <> show x

    -- From Account to Contract
    Prim.TransferTo  -> do
      let [assetExpr,holdingsExpr] = args

      senderAddr <- currentTxIssuer loc
      contractAddr <- currentAddress <$> ask
      assetAddr <- getAssetAddr assetExpr

      -- Eval and convert holdings val to integer
      holdingsVal <- evalLExpr holdingsExpr
      let holdings = holdingsValToBalance holdingsVal

      -- Modify the world (perform the transfer)
      world <- gets worldState
      case
        Ledger.transferAsset
          assetAddr
          (Asset.Holder senderAddr)
          (Asset.Holder contractAddr)
          holdings
          world
       of
        Left err -> throwError $ AssetIntegrity $ show err
        Right newWorld -> setWorld newWorld

      -- Emit the delta denoting the world state modification
      emitDelta $ Delta.ModifyAsset $
        Delta.TransferTo assetAddr holdings senderAddr contractAddr

      noop

    -- From Contract to Account
    Prim.TransferFrom  -> do
      let [assetExpr,holdingsExpr,accExpr] = args

      contractAddr <- currentAddress <$> ask
      assetAddr <- getAssetAddr assetExpr
      accAddr <- getAccountAddr accExpr

      -- Eval and convert holdings val to integer
      holdingsVal <- evalLExpr holdingsExpr
      let holdings = holdingsValToBalance holdingsVal

      -- Modify the world (perform the transfer)
      world <- gets worldState
      case
        Ledger.transferAsset
          assetAddr
          (Asset.Holder contractAddr)
          (Asset.Holder accAddr)
          holdings
          world
       of
        Left err -> throwError $ AssetIntegrity $ show err
        Right newWorld -> setWorld newWorld

      -- Emit the delta denoting the world state modification
      emitDelta $ Delta.ModifyAsset $
        Delta.TransferFrom assetAddr holdings accAddr contractAddr

      noop

    -- Circulate the supply of an Asset to the Asset issuer's holdings
    Prim.CirculateSupply -> do
      let [assetExpr, amountExpr] = args

      assetAddr <- getAssetAddr assetExpr

      -- Eval and convert amount val to integer
      amountVal <- evalLExpr amountExpr
      let amount = holdingsValToBalance amountVal

      -- Perform the circulation
      world <- gets worldState
      txIssuer <- currentTxIssuer loc
      case Ledger.circulateAsset assetAddr txIssuer amount world of
        Left err -> throwError $ AssetIntegrity $ show err
        Right newWorld -> setWorld newWorld

      noop

    -- From Account to Account
    Prim.TransferHoldings -> do
      let [fromExpr,assetExpr,holdingsExpr,toExpr] = args

      assetAddr <- getAssetAddr assetExpr
      fromAddr <- getAccountAddr fromExpr
      toAddr <- getAccountAddr toExpr

      -- Eval and convert holdings val to integer
      holdingsVal <- evalLExpr holdingsExpr
      let holdings = holdingsValToBalance holdingsVal

      -- Modify the world (perform the transfer)
      world <- gets worldState
      case
        Ledger.transferAsset
          assetAddr
          (Asset.Holder fromAddr)
          (Asset.Holder toAddr)
          holdings
          world
       of
        Left err -> throwError $ AssetIntegrity $ show err
        Right newWorld -> setWorld newWorld

      -- Emit the delta denoting the world state modification
      emitDelta $ Delta.ModifyAsset $
        Delta.TransferHoldings fromAddr assetAddr holdings toAddr

      noop

  where
    holdingsValToBalance :: Value -> Asset.Balance
    holdingsValToBalance = \case
      VNum (NumDecimal d) -> d
      VBool False         -> 0
      VBool True          -> 1
      x                   -> panicInvalidHoldingsVal x

evalMapPrim :: Prim.MapPrimOp -> [LExpr] -> EvalM Value
evalMapPrim mapPrimOp args =
  case mapPrimOp of
    Prim.MapInsert -> do
      [k, v, VMap mapVal] <- mapM evalLExpr args
      pure $ VMap (Map.insert k v mapVal)
    Prim.MapDelete -> do
      [k, VMap mapVal] <- mapM evalLExpr args
      pure $ VMap (Map.delete k mapVal)
    Prim.MapLookup -> do
      [k, VMap mapVal] <- mapM evalLExpr args
      case Map.lookup k mapVal of
        Nothing -> throwError $ LookupFail (show k)
        Just v  -> pure v
    Prim.MapModify -> do
      let [kexpr, Located _ (EVar lnm), mexpr] = args
      k <- evalLExpr kexpr
      VMap mapVal <- evalLExpr mexpr
      case Map.lookup k mapVal of
        Nothing -> throwError $ ModifyFail (show k)
        Just v  -> do
          helper <- lookupHelper lnm
          -- arity check passed in typechecker
          let [harg] = map (locVal . argName) (helperArgs helper)
          newVal <- localTempStorage [(harg,v)] $ evalLExpr (helperBody helper)
          pure $ VMap (Map.insert k newVal mapVal)

evalSetPrim :: Prim.SetPrimOp -> [LExpr] -> EvalM Value
evalSetPrim setPrimOp args =
  case setPrimOp of
    Prim.SetInsert -> do
      [v, VSet setVal] <- mapM evalLExpr args
      pure $ VSet (Set.insert v setVal)
    Prim.SetDelete -> do
      [v, VSet setVal] <- mapM evalLExpr args
      pure $ VSet (Set.delete v setVal)

evalCollPrim :: Prim.CollPrimOp -> [LExpr] -> EvalM Value
evalCollPrim collPrimOp args =
  case collPrimOp of
    Prim.Aggregate -> do
      let [vExpr, Located _ (EVar lnm), collExpr] = args
      initVal <- evalLExpr vExpr
      helper <- lookupHelper lnm
      collVal <- evalLExpr collExpr
      let [hargNm1, hargNm2] = map (locVal . argName) (helperArgs helper)
          foldColl' = foldColl (helperBody helper) (hargNm1, initVal) hargNm2
      case collVal of
        -- Currently, fold is performed over map vals ordered by keys
        VMap vmap -> foldColl' (Map.elems vmap)
        VSet vset -> foldColl' (Set.toList vset)
        otherwise -> throwInvalidCollErr (located collExpr) collVal
    Prim.Transform -> do
      let [Located _ (EVar lnm), collExpr] = args
      helper <- lookupHelper lnm
      collVal <- evalLExpr collExpr
      let [hargNm] = map (locVal . argName) (helperArgs helper)
      case collVal of
        VMap vmap -> VMap <$> mapColl (helperBody helper) hargNm vmap
        VSet vset -> VSet . Set.fromList <$>
          mapColl (helperBody helper) hargNm (Set.toList vset)
        otherwise -> throwInvalidCollErr (located collExpr) collVal
    Prim.Filter -> do
      let [Located _ (EVar lnm), collExpr] = args
      helper <- lookupHelper lnm
      collVal <- evalLExpr collExpr
      let [hargNm] = map (locVal . argName) (helperArgs helper)
      case collVal of
        VMap vmap ->
          fmap (VMap . Map.fromList) $
            flip filterM (Map.toList vmap) $ \(k,v) ->
              filterPred (helperBody helper) (hargNm,v)
        VSet vset ->
          fmap (VSet . Set.fromList) $
            flip filterM (Set.toList vset) $ \v ->
              filterPred (helperBody helper) (hargNm,v)
        otherwise -> throwInvalidCollErr (located collExpr) collVal
    Prim.Element -> do
      let [vExpr, collExpr] = args
      val <- evalLExpr vExpr
      collVal <- evalLExpr collExpr
      case collVal of
        VMap vmap -> pure (VBool (val `elem` vmap))
        VSet vset -> pure (VBool (val `elem` vset))
        otherwise -> throwInvalidCollErr (located collExpr) collVal
    Prim.IsEmpty -> do
      let [collExpr] = args
      collVal <- evalLExpr collExpr
      case collVal of
        VMap vmap -> pure (VBool (Map.null vmap))
        VSet vset -> pure (VBool (Set.null vset))
        otherwise -> throwInvalidCollErr (located collExpr) collVal
  where
    throwInvalidCollErr loc v = throwError $
      CallPrimOpFail loc (Just v) "Cannot call a collection primop on a non-collection value"

    -- Map over a collection type (which happen to all implement Traversable)
    mapColl :: Traversable f => LExpr -> Name -> f Value -> EvalM (f Value)
    mapColl body nm coll =
      forM coll $ \val ->
        localTempStorage [(nm, val)] (evalLExpr body)

    filterPred :: LExpr -> (Name, Value) -> EvalM Bool
    filterPred body var = do
      res <- localTempStorage [var] (evalLExpr body)
      pure $ case res of
        VBool True  -> True
        VBool False -> False
        otherwise -> panicImpossible "Body of helper function used in filter primop did not return Bool"

    foldColl :: LExpr -> (Name, Value) -> Name -> [Value] -> EvalM Value
    foldColl fbody (accNm, initVal) argNm vals =
        foldM accum initVal vals
      where
        accum accVal v =
          localTempStorage
            [(accNm, accVal), (argNm, v)]
            (evalLExpr fbody)

getAccountAddr :: LExpr -> EvalM Addr
getAccountAddr accExpr =
  Account.address <$> getAccount accExpr

getAccount :: LExpr -> EvalM Account.Account
getAccount accExpr = do
  ledgerState <- gets worldState
  accAddr <- extractAddrAccount <$> evalLExpr accExpr
  case Ledger.lookupAccount accAddr ledgerState of
    Left err  -> throwError $
      AccountIntegrity ("No account with address: " <> show accAddr)
    Right acc -> pure acc

getAssetAddr :: LExpr -> EvalM Addr
getAssetAddr assetExpr =
  Asset.address <$> getAsset assetExpr

getAsset :: LExpr -> EvalM Asset.Asset
getAsset assetExpr = do
  ledgerState <- gets worldState
  assetAddr   <- extractAddrAsset <$> evalLExpr assetExpr
  case Ledger.lookupAsset assetAddr ledgerState of
    Left err    -> throwError $
      AssetIntegrity ("No asset with address: " <> show assetAddr)
    Right asset -> pure asset

-- | Check that the method state precondition is a substate of the actual state.
checkGraph :: EvalM ()
checkGraph = do
  Just m <- gets currentMethod
  actualState <- getState
  unless (methodInputPlaces m `isSubWorkflow` actualState)
         (throwError $ StatePreconditionError (methodInputPlaces m) actualState)

-- | Does not perform typechecking on args supplied, eval should only happen
-- after typecheck. We don't check whether the input places are satisfied, since
-- this is done by 'Contract.callableMethods'
evalMethod :: Method -> [Value] -> EvalM Value
evalMethod meth@(Method _ _ nm argTyps body) args = do
    setCurrentMethod meth
    checkPreconditions meth
    checkGraph
    when (numArgs /= numArgsGiven)
         (throwError $ MethodArityError (locVal nm) numArgs numArgsGiven)
    forM_ (zip argNames args) . uncurry $ insertTempVar
    evalLExpr body
  where
    numArgs = length argTyps
    numArgsGiven = length args
    argNames = map (\(Arg _ lname) -> locVal lname) argTyps

-- | Evaluation entry
-- Methods will only ever be evaluated in the context of a contract on the
-- ledger. If script methods should be evaluated outside of the context of a
-- contract, call `evalMethod`.
eval :: Contract.Contract -> Name -> [Value] -> EvalM Value
eval c nm args =
  case Contract.lookupContractMethod nm c of
    Right method -> evalMethod method args
    Left err -> throwError (InvalidMethodName err)

noop :: EvalM Value
noop = pure VVoid


-- XXX find a better way to do this
extractAddrAccount :: Value -> Addr
extractAddrAccount (VAccount addr) = addr
extractAddrAccount _ = panicImpossible "extractAddrAccount"

extractAddrAsset :: Value -> Addr
extractAddrAsset (VAsset addr) = addr
extractAddrAsset _ = panicImpossible "extractAddrAsset"

extractAddrContract :: Value -> Addr
extractAddrContract (VContract addr) = addr
extractAddrContract _ = panicImpossible "extractAddrContract"

-------------------------------------------------------------------------------
-- Method Precondition Evaluation and Checking
-------------------------------------------------------------------------------

-- Evaluated preconditions
data PreconditionsV = PreconditionsV
  { afterV :: Maybe DateTime
  , beforeV :: Maybe DateTime
  , roleV :: Maybe (Set (Addr))
  }

-- | Succeeds when all preconditions are fulfilled and throws an error otherwise
-- NB: We assume that we are given a type checked AST
evalPreconditions :: Method -> EvalM PreconditionsV
evalPreconditions m = do
  let Preconditions ps = methodPreconditions m
  PreconditionsV
    <$> sequence (evalAfter <$> List.lookup PrecAfter ps)
    <*> sequence (evalBefore <$> List.lookup PrecBefore ps)
    <*> sequence (evalRole <$> List.lookup PrecRoles ps)
  where
    evalAfter :: LExpr -> EvalM DateTime
    evalAfter expr = do
      VDateTime dt <- evalLExpr expr
      pure dt

    evalBefore :: LExpr -> EvalM DateTime
    evalBefore expr = do
      VDateTime dt <- evalLExpr expr
      pure dt

    evalRole :: LExpr -> EvalM (Set (Addr))
    evalRole expr = do
      VSet vAccounts <- evalLExpr expr
      let accounts = Set.map (\(VAccount a) -> a) vAccounts
      pure accounts

checkPreconditions :: Method -> EvalM ()
checkPreconditions m = do
  PreconditionsV afterV beforeV roleV <- evalPreconditions m
  sequence_
    [ sequence (checkAfter <$> afterV)
    , sequence (checkBefore <$> beforeV)
    , sequence (checkRole <$> roleV)
    ]
  where
    checkAfter dt = do
      now <- DateTime . posixMicroSecsToDatetime <$> currBlockTimestamp NoLoc
      unless
        (now >= dt)
        (throwError $ PrecNotSatAfter m dt now)

    checkBefore dt = do
      now <- DateTime . posixMicroSecsToDatetime <$> currBlockTimestamp NoLoc
      unless
        (now < dt)
        (throwError $ PrecNotSatBefore m dt now)

    checkRole accounts = do
      issuer <- currentTxIssuer NoLoc
      unless
        (issuer `elem` accounts)
        (throwError $ PrecNotSatCaller m accounts issuer)

evalCallableMethods :: Contract.Contract -> EvalM Contract.CallableMethods
evalCallableMethods contract =
    foldM insertCallableMethod mempty (Contract.callableMethods contract)
  where
    insertCallableMethod cms method = do
      PreconditionsV afterV beforeV roleV <- evalPreconditions method
      withinTime <-
        case (afterV, beforeV) of
          (Nothing, Nothing) -> pure True
          (_,_) -> do
            -- Only read the current block time if there are temporal preconditions
            now <- DateTime . posixMicroSecsToDatetime <$> currBlockTimestamp NoLoc
            let isAfter = maybe True (\dt -> now >= dt) afterV
                isBefore = maybe True (\dt -> now < dt) beforeV
            pure (isAfter && isBefore)
      if not withinTime
        then pure cms -- don't add to callable methods since not callable at this time
        else do
          let group = case roleV of
                Nothing -> Contract.Anyone
                Just accounts -> Contract.Restricted accounts
          pure $ Map.insert (locVal $ methodName method) (group, argtys method) cms

-------------------------------------------------------------------------------
-- Value Hashing
-------------------------------------------------------------------------------

{-# INLINE hashValue #-}
hashValue :: Value -> EvalM ByteString
hashValue = \case
  VText msg      -> pure $ SS.toBytes msg
  VNum n         -> pure (show n)
  VBool n        -> pure (show n)
  VState n       -> pure (show n)
  VAccount a     -> pure (rawAddr a)
  VContract a    -> pure (rawAddr a)
  VAsset a       -> pure (rawAddr a)
  VVoid          -> pure ""
  VDateTime dt   -> pure $ S.encode dt
  VTimeDelta d   -> pure $ S.encode d
  VEnum c        -> pure (show c)
  VMap vmap      -> pure (show vmap)
  VSet vset      -> pure (show vset)
  VSig _         -> throwError $ Impossible "Cannot hash signature"
  VUndefined     -> throwError $ Impossible "Cannot hash undefined"

-------------------------------------------------------------------------------
  -- Eval specific fatal errors
-------------------------------------------------------------------------------

panicInvalidBinOp :: BinOp -> Value -> Value -> a
panicInvalidBinOp op x y = panicImpossible $
  "Operator " <> show op <> " cannot be used with " <> show x <> " and " <> show y

panicInvalidUnOp :: UnOp -> Value -> a
panicInvalidUnOp op x = panicImpossible $
  "Operator " <> show op <> " cannot be used with " <> show x

panicInvalidHoldingsVal :: Value -> a
panicInvalidHoldingsVal v = panicImpossible $
  "Only numbers and booleans can be values of asset holdings. Instead, saw: "  <> show v
