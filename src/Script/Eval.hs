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

import Prelude (read)

import Control.Monad.Fail

import Fixed
import Script
import SafeInteger
import SafeString as SS
-- import Key (PrivateKey)
import Time (Timestamp, posixMicroSecsToDatetime)
-- import Ledger (World)
import Storage
-- import Account (Account,  address, publicKey)
import Script.Error as Error
import Script.Prim (PrimOp(..))
-- import Address (Address, AContract, AAccount, AAsset, rawAddr)
import Script.ReachabilityGraph (applyTransition)
import Utils (panicImpossible)
import qualified Delta
import qualified Contract
import qualified Hash
-- import qualified Key
import qualified Ledger
import qualified Script.Pretty as Pretty
import qualified Script.Prim as Prim
import Utils (traverseWithKey')

import qualified Datetime as DT
import Datetime.Types (within, Interval(..), add, sub, subDeltas, scaleDelta)
import qualified Datetime.Types as DT

import Data.Fixed (Fixed(..), showFixed)
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
data TransactionCtx ac = TransactionCtx
  { transactionHash     :: Hash.Hash Encoding.Base16ByteString -- ^ Hash of the transaction
  , transactionIssuer   :: ac                    -- ^ Issuer of the transaction
  , transactionBlockIdx :: Int64                               -- ^ Index of the block in which the transaction is contained
  , transactionBlockTs  :: Time.Timestamp                      -- ^ The timestamp of the block in which the transaction is contained
  } deriving (Generic, NFData)

-- | Evaluation context used during remote evaluation in a validating engine.
data EvalCtx as ac c sk = EvalCtx
  { currentValidator   :: ac   -- ^ Referencing an account
  , currentTxCtx       :: Maybe (TransactionCtx ac) -- ^ Information about the current transaction
  , currentCreated     :: Time.Timestamp    -- ^ When the contract was deployed
  , currentDeployer    :: ac  -- ^ Referencing an account
  , currentAddress     :: c -- ^ Address of current Contract
  , currentPrivKey     :: sk    -- ^ Private key of Validating node
  , currentHelpers     :: [Helper as ac c]
  -- ^ Script helper functions available for call in method body
  } deriving (Generic, NFData)

type LocalStorages as ac c = Map.Map ac (Storage as ac c)

data EvalState as ac c asset account = EvalState
  { tempStorage      :: Storage as ac c          -- ^ Tmp variable env
  , globalStorage    :: Storage as ac c          -- ^ Global variable env
  , workflowState    :: WorkflowState    -- ^ Current state of contract
  , currentMethod    :: Maybe (Method as ac c)     -- ^ Which method we're currently in, if any
  , worldState       :: Ledger.World as ac c asset account            -- ^ Current world state
  , deltas           :: [Delta.Delta as ac c]
  } deriving (Generic, NFData)

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

initEvalState
  :: Contract.Contract as ac c
  -> Ledger.World as ac c asset account
  -> EvalState as ac c asset account
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

lookupGlobalVar :: Name -> (EvalM as ac c asset account sk) (Maybe (Value as ac c))
lookupGlobalVar (Name var) = do
  globalStore <- gets globalStorage
  return $ Map.lookup (Key var) globalStore

lookupTempVar :: Name -> (EvalM as ac c asset account sk) (Maybe (Value as ac c))
lookupTempVar (Name var) = do
  tmpStore <- gets tempStorage
  return $ Map.lookup (Key var) tmpStore

insertTempVar :: Name -> (Value as ac c) -> (EvalM as ac c asset account sk) ()
insertTempVar (Name var) val = modify' $ \evalState ->
    evalState { tempStorage = insertVar (tempStorage evalState) }
  where
    insertVar = Map.insert (Key var) val

-- | Extends the temp storage with temporary variable updates. Emulates a
-- closure environment for evaluating the body of helper functions by
-- assigning values to argument names. Effectively ad-hoc substitution.
localTempStorage :: [(Name, Value as ac c)] -> (EvalM as ac c asset account sk) a -> (EvalM as ac c asset account sk) a
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
lookupHelper :: LName -> (EvalM as ac c asset account sk) (Helper as ac c)
lookupHelper lhnm = do
  helpers <- currentHelpers <$> ask
  case List.find ((==) lhnm . helperName) helpers of
    Nothing     -> panicImpossible $ Just "lookupHelper: Undefined helper function name"
    Just helper -> pure helper

-- | Emit a delta updating  the state of a global reference.
updateGlobalVar :: Name -> (Value as ac c) -> (EvalM as ac c asset account sk) ()
updateGlobalVar (Name var) val = modify' $ \evalState ->
    evalState { globalStorage = updateVar (globalStorage evalState) }
  where
    updateVar = Map.update (\_ -> Just val) (Key var)

setWorld :: Ledger.World as ac c asset account -> (EvalM as ac c asset account sk) ()
setWorld w = modify' $ \evalState -> evalState { worldState = w }

-- | Update the evaluate state.
updateState :: WorkflowState -> (EvalM as ac c asset account sk) ()
updateState newState = modify' $ \s -> s { workflowState = newState }

-- | Get the evaluation state
getState :: (EvalM as ac c asset account sk) WorkflowState
getState = gets workflowState

setCurrentMethod :: Method as ac c -> (EvalM as ac c asset account sk) ()
setCurrentMethod m = modify' $ \s -> s { currentMethod = Just m }

-- | Emit a delta
emitDelta :: Delta.Delta as ac c -> (EvalM as ac c asset account sk) ()
emitDelta delta = modify' $ \s -> s { deltas = deltas s ++ [delta] }

-- | Lookup variable in scope
lookupVar :: Name -> (EvalM as ac c asset account sk) (Maybe (Value as ac c))
lookupVar var = do
  gVar <- lookupGlobalVar var
  case gVar of
    Nothing  -> lookupTempVar var
    Just val -> return $ Just val

transactionCtxField :: Loc -> Text -> (TransactionCtx ac -> a) -> (EvalM as ac c asset account sk) a
transactionCtxField loc errMsg getField = do
  mfield <- fmap getField . currentTxCtx <$> ask
  case mfield of
    Nothing -> throwError $ NoTransactionContext loc errMsg
    Just field -> pure field

currBlockTimestamp :: Loc -> (EvalM as ac c asset account sk) Time.Timestamp
currBlockTimestamp loc = do
  let errMsg = "Cannot get timestamp without a transaction context"
  transactionCtxField loc errMsg transactionBlockTs

currentBlockIdx :: Loc -> (EvalM as ac c asset account sk) Int64
currentBlockIdx loc = do
  let errMsg = "Cannot get block index without a transaction context"
  transactionCtxField loc errMsg transactionBlockIdx

currentTxHash
  :: Loc
  -> (EvalM as ac c asset account sk) (Hash.Hash Encoding.Base16ByteString)
currentTxHash loc = do
  let errMsg = "Cannot get current transaction hash without a transaction context"
  transactionCtxField loc errMsg transactionHash

currentTxIssuer :: Loc -> (EvalM as ac c asset account sk) ac
currentTxIssuer loc = do
  let errMsg = "Cannot get current transaction issuer without a transaction context"
  transactionCtxField loc errMsg transactionIssuer

-------------------------------------------------------------------------------
-- Interpreter Monad
-------------------------------------------------------------------------------

type RandomM = Crypto.MonadPseudoRandom Crypto.SystemDRG

-- TODO: Fix the MonadFail issues
instance MonadFail RandomM where

-- | Initialize the random number generator and run the monadic
-- action.
runRandom :: RandomM a -> IO a
runRandom m = do
  gen <- Crypto.getSystemDRG
  return . fst . Crypto.withDRG gen $ m

-- | EvalM monad
type EvalM as ac c asset account sk
  = ReaderT
    (EvalCtx as ac c sk)
    (StateT (EvalState as ac c asset account) (ExceptT (Error.EvalFail as ac c) RandomM))

instance Crypto.MonadRandom (EvalM as ac c asset account sk) where
  getRandomBytes = lift . lift . lift . Crypto.getRandomBytes

-- | Run the evaluation monad.
execEvalM
  :: EvalCtx as ac c sk
  -> EvalState as ac c asset account
  -> (EvalM as ac c asset account sk) a
  -> IO (Either (Error.EvalFail as ac c) (EvalState as ac c asset account))
execEvalM evalCtx evalState
  = handleArithError
  . runRandom
  . runExceptT
  . flip execStateT evalState
  . flip runReaderT evalCtx

-- | Run the evaluation monad.
runEvalM
  :: EvalCtx as ac c sk
  -> EvalState as ac c asset account
  -> (EvalM as ac c asset account sk) a
  -> IO (Either (Error.EvalFail as ac c) (a, EvalState as ac c asset account))
runEvalM evalCtx evalState
  = handleArithError
  . runRandom
  . runExceptT
  . flip runStateT evalState
  . flip runReaderT evalCtx

handleArithError
  :: IO (Either (Error.EvalFail as ac c) a)
  -> IO (Either (Error.EvalFail as ac c) a)
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
evalLExpr
  :: forall as ac c asset account sk.
  (Eq as, Eq ac, Eq c, Ord as, Ord ac, Ord c, Show as, Show ac, Show c
  ,Ledger.Addressable asset, Ledger.Addressable account)
  => LExpr as ac c
  -> (EvalM as ac c asset account sk) (Value as ac c)
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
      _ -> panicImpossible $ Just "EUnOp"

  -- This logic handles the special cases of operating over homomorphic
  -- crypto-text.
  EBinOp (Located _ op) a b -> do
    valA <- evalLExpr a
    valB <- evalLExpr b
    let binOpFail = panicInvalidBinOp op valA valB
    case (valA, valB) of
      (VInt a', VInt b') ->
        case op of
          Script.Add -> pure $ VInt (a' + b')
          Script.Sub -> pure $ VInt (a' - b')
          Script.Div ->
            if b' == 0
              then throwError DivideByZero
              else pure $ VInt (a' `div` b')
          Script.Mul     -> pure $ VInt (a' * b')
          Script.Equal   -> pure $ VBool $ a' == b'
          Script.NEqual  -> pure $ VBool $ a' /= b'
          Script.LEqual  -> pure $ VBool $ a' <= b'
          Script.GEqual  -> pure $ VBool $ a' >= b'
          Script.Lesser  -> pure $ VBool $ a' < b'
          Script.Greater -> pure $ VBool $ a' > b'
          _ -> binOpFail
      (VFloat a', VFloat b') -> evalBinOpF op VFloat a' b'
      (VFixed a', VFixed b') ->
        case (a',b') of
          (Fixed1 x, Fixed1 y) -> evalBinOpF op (VFixed . Fixed1) x y
          (Fixed2 x, Fixed2 y) -> evalBinOpF op (VFixed . Fixed2) x y
          (Fixed3 x, Fixed3 y) -> evalBinOpF op (VFixed . Fixed3) x y
          (Fixed4 x, Fixed4 y) -> evalBinOpF op (VFixed . Fixed4) x y
          (Fixed5 x, Fixed5 y) -> evalBinOpF op (VFixed . Fixed5) x y
          (Fixed6 x, Fixed6 y) -> evalBinOpF op (VFixed . Fixed6) x y
          (_,_) -> binOpFail
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
      (VTimeDelta (TimeDelta d), VInt n) ->
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
      (v1, v2) -> panicImpossible $ Just $
        "evalLExpr EBinOp: (" <> show v1 <> ", " <> show v2 <> ")"

  EVar (Located _ var) -> do
    mVal <- lookupVar var
    case mVal of
      Nothing -> panicImpossible $ Just "evalLExpr: EVar"
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
    p <- evalPrim loc Between [nowDtLExpr, startDte, endDte]
    case p of
      VBool b -> if b
        then evalLExpr e
        else noop
      _ -> panicImpossible . Just $ "Evaluating between start/end date expression at " <> show loc

  EIf cond e1 e2 -> do
    p <- evalLExpr cond
    case p of
      VBool b -> if b
        then evalLExpr e1
        else evalLExpr e2
      _ -> panicImpossible . Just $ "Evaluating conditional expression at " <> show loc

  ECase scrut ms -> do
    p <- evalLExpr scrut
    case p of
      VEnum c -> evalLExpr (match ms c)
      _ -> panicImpossible . Just $ "Evaluating case expression at " <> show loc

  ENoOp -> noop

  EMap m ->
    VMap <$> traverseWithKey' (\k v -> (,) <$> evalLExpr k <*> evalLExpr v) m

  ESet s ->
    VSet . Set.fromList <$> traverse evalLExpr (toList s)

  EHole ->
    panicImpossible . Just $ "Evaluating hole expression at " <> show loc

match :: [Match as ac c] -> EnumConstr -> LExpr as ac c
match ps c
  = fromMaybe (panicImpossible $ Just "Cannot match constructor")
  $ List.lookup (PatLit c)
  $ map (\(Match pat body) -> (locVal pat, body))
  $ ps

-- | Evaluate a binop and two Fractional Num args
evalBinOpF
  :: (Fractional a, Ord a, Show as, Show ac, Show c)
  => BinOp
  -> (a -> Value as ac c)
  -> a
  -> a
  -> (EvalM as ac c asset account sk) (Value as ac c)
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

evalPrim
  :: forall as ac c asset account sk.
  (Show as, Show ac, Show c, Ord as, Ord ac, Ord c, Ledger.Addressable asset, Ledger.Addressable account)
  => Loc
  -> PrimOp
  -> [LExpr as ac c]
  -> (EvalM as ac c asset account sk) (Value as ac c)
evalPrim loc ex args = case ex of
  Now               -> do
    currDatetime <- posixMicroSecsToDatetime <$> currBlockTimestamp loc
    pure $ VDateTime $ DateTime currDatetime
  Block             -> VInt <$> currentBlockIdx loc
  Deployer          -> VAccount . currentDeployer <$> ask
  Sender            -> VAccount <$> currentTxIssuer loc
  Created           -> do
    createdDatetime <- posixMicroSecsToDatetime . currentCreated <$> ask
    pure $ VDateTime $ DateTime createdDatetime
  Address           -> VContract . currentAddress <$> ask
  Validator         -> VAccount . currentValidator <$> ask

  Fixed1ToFloat  -> do
    let [eFixed] = args
    p <- evalLExpr eFixed
    case p of
      VFixed (Fixed1 (F1 n))
        -> pure $ VFloat $ read $ showFixed False n
      _ -> panicImpossible . Just $ "Evaluating fixed to float expression"

  Fixed2ToFloat  -> do
    let [eFixed] = args
    p <- evalLExpr eFixed
    case p of
      VFixed (Fixed2 (F2 n))
        -> pure $ VFloat $ read $ showFixed False n
      _ -> panicImpossible . Just $ "Evaluating fixed to float expression"

  Fixed3ToFloat  -> do
    let [eFixed] = args
    p <- evalLExpr eFixed
    case p of
      VFixed (Fixed3 (F3 n))
        -> pure $ VFloat $ read $ showFixed False n
      _ -> panicImpossible . Just $ "Evaluating fixed to float expression"

  Fixed4ToFloat  -> do
    let [eFixed] = args
    p <- evalLExpr eFixed
    case p of
      VFixed (Fixed4 (F4 n))
        -> pure $ VFloat $ read $ showFixed False n
      _ -> panicImpossible . Just $ "Evaluating fixed to float expression"

  Fixed5ToFloat  -> do
    let [eFixed] = args
    p <- evalLExpr eFixed
    case p of
      VFixed (Fixed5 (F5 n))
        -> pure $ VFloat $ read $ showFixed False n
      _ -> panicImpossible . Just $ "Evaluating fixed to float expression"

  Fixed6ToFloat  -> do
    let [eFixed] = args
    p <- evalLExpr eFixed
    case p of
      VFixed (Fixed6 (F6 n))
        -> pure $ VFloat $ read $ showFixed False n
      _ -> panicImpossible . Just $ "Evaluating fixed to float expression"

  FloatToFixed1  -> evalFloatToFixed Prec1 args
  FloatToFixed2  -> evalFloatToFixed Prec2 args
  FloatToFixed3  -> evalFloatToFixed Prec3 args
  FloatToFixed4  -> evalFloatToFixed Prec4 args
  FloatToFixed5  -> evalFloatToFixed Prec5 args
  FloatToFixed6  -> evalFloatToFixed Prec6 args

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
      Nothing -> panicImpossible $ Just "evalPrim: transitionTo can only be called from a method body."
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
          Just (Left err) -> panicImpossible . Just $
            "evalPrim: how did you manage to smuggle an unsound workflow past the soundness checker?\n"
            <> Pretty.prettyPrint err
          Nothing -> panicImpossible . Just . Pretty.prettyPrint . Pretty.sep $
            [ "evalPrim: how did you manage to smuggle an unsound workflow past the soundness checker?"
            , Pretty.squotes (Pretty.ppr t)
            , "is not enabled in current global state"
            , Pretty.squotes (Pretty.ppr oldGlobalState)
            ]

  CurrentState -> VState <$> getState

  Stay -> do
    m <- gets currentMethod
    case m of
      Nothing -> panicImpossible $ Just "evalPrim: stay can only be called from a method body."
      Just m -> pure VVoid

  Sign           -> do
    let [msgExpr] = args
    p <- evalLExpr msgExpr
    case p of
      VText msg -> do
        privKey <- currentPrivKey <$> ask -- XXX               V gen Random value?
        sig <- notImplemented
        -- Key.getSignatureRS <$> Key.sign privKey (SS.toBytes msg)
        case bimap toSafeInteger toSafeInteger sig of
          (Right safeR, Right safeS) -> return $ VSig (safeR,safeS)
          otherwise -> throwError $
            HugeInteger "Signature values (r,s) too large."
      _ -> panicImpossible . Just $ "Evaluation of signature"


  Sha256         -> do
    let [anyExpr] = args
    v <- hashValue =<< evalLExpr anyExpr
    case SS.fromBytes (Hash.sha256Raw v) of
      Left err -> throwError $ HugeString $ show err
      Right msg -> return $ VText msg

  AccountExists  -> do
    let [varExpr] = args
    accAddr <- extractAddrAccount <$> evalLExpr varExpr
    world <- gets worldState
    pure $ VBool $ isRight $ Ledger.lookupAccount world accAddr

  AssetExists    -> do
    let [varExpr] = args
    assetAddr <- extractAddrAsset <$> evalLExpr varExpr
    world <- gets worldState
    pure $ VBool $ isRight $ Ledger.lookupAsset world assetAddr

  ContractExists -> do
    let [varExpr] = args
    contractAddr <- extractAddrContract <$> evalLExpr varExpr
    world <- gets worldState
    pure $ VBool $ isRight $ Ledger.lookupContract world contractAddr

  TxHash -> do
    txHash <- currentTxHash loc
    case SS.fromBytes (Hash.getRawHash txHash) of
      Left err -> throwError $ HugeString $ show err
      Right msg -> pure $ VText msg

  ContractValue -> do
    let [contractExpr, msgExpr] = args
    contractAddr <- extractAddrContract <$> evalLExpr contractExpr
    world <- gets worldState
    case Ledger.lookupContract world contractAddr of
      Left err -> throwError $ ContractIntegrity $ show err
      Right contract -> do
        p <- evalLExpr msgExpr
        case p of
          VText varSS -> do
            let var = toS $ SS.toBytes varSS
            case Contract.lookupVarGlobalStorage var contract of
              Nothing -> throwError $ ContractIntegrity $
                "Contract does not define a variable named '" <> var <> "'"
              Just val -> pure val
          _ -> panicImpossible . Just $ "Evaluating contract value"

  ContractValueExists -> do
    -- If ContractValue throws err, value doesn't exist
    flip catchError (const $ pure $ VBool False) $ do
      _ <- evalPrim loc ContractValue args
      pure $ VBool True

  ContractState -> do
    let [contractExpr] = args
    contractAddr <- extractAddrContract <$> evalLExpr contractExpr
    world <- gets worldState
    case Ledger.lookupContract world contractAddr of
      Left err -> throwError . ContractIntegrity $ show err
      Right contract -> pure . VState $ Contract.state contract

  -- Datetime manipulation prim ops

  IsBusinessDayUK -> do
    let [dateTimeExpr] = args
    VDateTime (DateTime dt) <- evalLExpr dateTimeExpr
    return $ VBool $ DT.isBusiness DT.ukHolidays dt

  NextBusinessDayUK -> do
    let [dateTimeExpr] = args
    p <- evalLExpr dateTimeExpr
    case p of
     VDateTime (DateTime dt)
       -> return $ VDateTime $ DateTime $ DT.nextBusinessDay DT.ukHolidays dt
     _ -> panicImpossible . Just $ "Evaluating next business day UK"

  IsBusinessDayNYSE -> do
    let [dateTimeExpr] = args
    p <- evalLExpr dateTimeExpr
    case p of
     VDateTime (DateTime dt)
       -> return $ VBool $ DT.isBusiness DT.nyseHolidays dt
     _ -> panicImpossible . Just $ "Evaluating next business day NYSE"

  NextBusinessDayNYSE -> do
    let [dateTimeExpr] = args
    p <- evalLExpr dateTimeExpr
    case p of
     VDateTime (DateTime dt)
       -> return $ VDateTime $ DateTime $ DT.nextBusinessDay DT.nyseHolidays dt
     _ -> panicImpossible . Just $ "Evaluating next business day NYSE"

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

evalAssetPrim
  :: forall as ac c asset account sk.
  (Ord as, Ord ac, Ord c, Show as, Show ac, Show c
  ,Ledger.Addressable asset, Ledger.Addressable account)
  => Loc
  -> Prim.AssetPrimOp
  -> [LExpr as ac c]
  -> (EvalM as ac c asset account sk) (Value as ac c)
evalAssetPrim loc assetPrimOp args =
  case assetPrimOp of

    -- May return VInt, VBool, or VFixed depending on asset type
    Prim.HolderBalance -> do
      let [assetExpr, accExpr] = args
      asset <- getAsset assetExpr
      accAddr <- getAccountAddr accExpr
      world <- gets worldState
      pure $ Ledger.calcBalance world asset accAddr
      -- case Asset.assetType asset of
      --   Asset.Discrete  ->
      --     case Asset.balance asset accAddr of
      --       Nothing  -> return $ VInt 0
      --       Just bal -> return $ VInt bal
      --   Asset.Fractional n ->
      --     case Asset.balance asset accAddr of
      --       Nothing  -> return $ VFloat 0.0
      --       Just bal -> return $ VFloat $
      --         -- normalize the holdings amount by `bal * 10^(-n)`
      --         fromIntegral bal * 10**(fromIntegral $ negate $ fromEnum n + 1)
      --   Asset.Binary ->
      --     case Asset.balance asset accAddr of
      --       Nothing  -> return $ VBool False
      --       Just bal -> return $ VBool True

    -- From Account to Contract
    Prim.TransferTo  -> do
      let [assetExpr,holdingsExpr] = args

      senderAddr <- currentTxIssuer loc
      contractAddr <- currentAddress <$> ask
      assetAddr <- getAssetAddr assetExpr

      -- Eval and convert holdings val to integer
      holdingsVal <- evalLExpr holdingsExpr
      let holdings = holdingsValToInteger holdingsVal

      -- Modify the world (perform the transfer)
      world <- gets worldState
      case Ledger.transferAsset world assetAddr senderAddr contractAddr holdings of
        Left err -> throwError $ AssetIntegrity $ show err
        Right newWorld -> setWorld newWorld

      -- Emit the delta denoting the world state modification
      emitDelta $ Delta.ModifyAsset $
        Delta.TransferTo assetAddr holdings senderAddr contractAddr

      noop

    -- From Contract to Account
    Prim.TransferFrom  -> do
      notImplemented
      -- let [assetExpr,holdingsExpr,accExpr] = args

      -- contractAddr <- currentAddress <$> ask
      -- assetAddr <- getAssetAddr assetExpr
      -- accAddr <- getAccountAddr accExpr

      -- -- Eval and convert holdings val to integer
      -- holdingsVal <- evalLExpr holdingsExpr
      -- let holdings = holdingsValToInteger holdingsVal

      -- -- Modify the world (perform the transfer)
      -- world <- gets worldState
      -- case Ledger.transferAsset world assetAddr contractAddr accAddr holdings of
      --   Left err -> throwError $ AssetIntegrity $ show err
      --   Right newWorld -> setWorld newWorld

      -- -- Emit the delta denoting the world state modification
      -- emitDelta $ Delta.ModifyAsset $
      --   Delta.TransferFrom assetAddr holdings accAddr contractAddr

      -- noop

    -- Circulate the supply of an Asset to the Asset issuer's holdings
    Prim.CirculateSupply -> do
      let [assetExpr, amountExpr] = args

      assetAddr <- getAssetAddr assetExpr

      -- Eval and convert amount val to integer
      amountVal <- evalLExpr amountExpr
      let amount = holdingsValToInteger amountVal

      -- Perform the circulation
      world <- gets worldState
      txIssuer <- currentTxIssuer loc
      case Ledger.circulateAsset world assetAddr txIssuer amount of
        Left err -> throwError $ AssetIntegrity $ show err
        Right newWorld -> setWorld newWorld

      noop

    -- From Account to Account
    Prim.TransferHoldings -> do
      notImplemented
      -- let [fromExpr,assetExpr,holdingsExpr,toExpr] = args

      -- assetAddr <- getAssetAddr assetExpr
      -- fromAddr <- getAccountAddr fromExpr
      -- toAddr <- getAccountAddr toExpr

      -- -- Eval and convert holdings val to integer
      -- holdingsVal <- evalLExpr holdingsExpr
      -- let holdings = holdingsValToInteger holdingsVal

      -- -- Modify the world (perform the transfer)
      -- world <- gets worldState
      -- case Ledger.transferAsset world assetAddr fromAddr toAddr holdings of
      --   Left err -> throwError $ AssetIntegrity $ show err
      --   Right newWorld -> setWorld newWorld

      -- -- Emit the delta denoting the world state modification
      -- emitDelta $ Delta.ModifyAsset $
      --   Delta.TransferHoldings fromAddr assetAddr holdings toAddr

      -- noop

  where
    holdingsValToInteger :: Value as ac c -> Int64
    holdingsValToInteger = \case
      VInt n   -> n
      VBool b  -> if b then 1 else 0
      VFixed f -> fromIntegral $ getFixedInteger f
      otherVal -> panicInvalidHoldingsVal otherVal

    getAccountAddr :: LExpr as ac c -> (EvalM as ac c asset account sk) ac
    getAccountAddr accExpr =
      Ledger.toAddress <$> getAccount accExpr

    getAccount :: LExpr as ac c -> (EvalM as ac c asset account sk) account
    getAccount accExpr = do
      world <- gets worldState
      accAddr <- extractAddrAccount <$> evalLExpr accExpr
      case Ledger.lookupAccount world accAddr of
        Left err  -> throwError $
          AccountIntegrity ("No account with address: " <> show accAddr)
        Right acc -> pure acc

    getAssetAddr :: LExpr as ac c -> (EvalM as ac c asset account sk) as
    getAssetAddr assetExpr =
      Ledger.toAddress <$> getAsset assetExpr

    getAsset :: LExpr as ac c -> (EvalM as ac c asset account sk) asset
    getAsset assetExpr = do
      world <- gets worldState
      assetAddr   <- extractAddrAsset <$> evalLExpr assetExpr
      case Ledger.lookupAsset world assetAddr of
        Left err    -> throwError $
          AssetIntegrity ("No asset with address: " <> show assetAddr)
        Right asset -> pure asset

evalMapPrim
  :: forall as ac c asset account sk.
  (Ord as, Ord ac, Ord c, Show as, Show ac, Show c
  ,Ledger.Addressable asset, Ledger.Addressable account)
  => Prim.MapPrimOp
  -> [LExpr as ac c]
  -> (EvalM as ac c asset account sk) (Value as ac c)
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

evalSetPrim
  :: (Ord as, Ord ac, Ord c, Show as, Show ac, Show c
     ,Ledger.Addressable asset, Ledger.Addressable account)
  => Prim.SetPrimOp -> [LExpr as ac c] -> (EvalM as ac c asset account sk) (Value as ac c)
evalSetPrim setPrimOp args =
  case setPrimOp of
    Prim.SetInsert -> do
      [v, VSet setVal] <- mapM evalLExpr args
      pure $ VSet (Set.insert v setVal)
    Prim.SetDelete -> do
      [v, VSet setVal] <- mapM evalLExpr args
      pure $ VSet (Set.delete v setVal)

evalCollPrim
  :: forall as ac c asset account sk.
  (Ord as, Ord ac, Ord c, Show as, Show ac, Show c
  ,Ledger.Addressable asset, Ledger.Addressable account)
  => Prim.CollPrimOp -> [LExpr as ac c] -> (EvalM as ac c asset account sk) (Value as ac c)
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
    mapColl :: Traversable f => LExpr as ac c -> Name -> f (Value as ac c) -> (EvalM as ac c asset account sk) (f (Value as ac c))
    mapColl body nm coll =
      forM coll $ \val ->
        localTempStorage [(nm, val)] (evalLExpr body)

    filterPred :: LExpr as ac c -> (Name, Value as ac c) -> (EvalM as ac c asset account sk) Bool
    filterPred body var = do
      res <- localTempStorage [var] (evalLExpr body)
      pure $ case res of
        VBool True  -> True
        VBool False -> False
        otherwise -> panicImpossible $ Just "Body of helper function used in filter primop did not return Bool"

    foldColl :: LExpr as ac c -> (Name, Value as ac c) -> Name -> [Value as ac c] -> (EvalM as ac c asset account sk) (Value as ac c)
    foldColl fbody (accNm, initVal) argNm vals =
        foldM accum initVal vals
      where
        accum accVal v =
          localTempStorage
            [(accNm, accVal), (argNm, v)]
            (evalLExpr fbody)


-- | Check that the method state precondition is a substate of the actual state.
checkGraph :: (EvalM as ac c asset account sk) ()
checkGraph = do
  Just m <- gets currentMethod
  actualState <- getState
  unless (methodInputPlaces m `isSubWorkflow` actualState)
         (throwError $ StatePreconditionError (methodInputPlaces m) actualState)

-- | Does not perform typechecking on args supplied, eval should only happen
-- after typecheck. We don't check whether the input places are satisfied, since
-- this is done by 'Contract.callableMethods'
evalMethod
  :: (Ord as, Ord ac, Ord c, Show as, Show ac, Show c
     ,Ledger.Addressable asset, Ledger.Addressable account)
  => Method as ac c -> [Value as ac c] -> (EvalM as ac c asset account sk) (Value as ac c)
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
eval
  :: (Eq as, Eq ac, Eq c, Ord as, Ord ac, Ord c, Show as, Show ac, Show c
     ,Ledger.Addressable asset, Ledger.Addressable account)
  => Contract.Contract as ac c
  -> Name
  -> [Value as ac c]
  -> (EvalM as ac c asset account sk) (Value as ac c)
eval c nm args =
  case Contract.lookupContractMethod nm c of
    Right method -> evalMethod method args
    Left err -> throwError (InvalidMethodName err)

evalFloatToFixed :: (Ord as, Ord ac, Ord c, Show as, Show ac, Show c
                    ,Ledger.Addressable asset, Ledger.Addressable account) => PrecN -> [LExpr as ac c] -> (EvalM as ac c asset account sk) (Value as ac c)
evalFloatToFixed prec args = do
    let [eFloat] = args
    VFloat float <- evalLExpr eFloat
    pure $ VFixed $ floatToFixed prec float
  where
    floatToFixed :: PrecN -> Double -> FixedN
    floatToFixed Prec1 = Fixed1 . F1 . MkFixed . round . (*) (10^1)
    floatToFixed Prec2 = Fixed2 . F2 . MkFixed . round . (*) (10^2)
    floatToFixed Prec3 = Fixed3 . F3 . MkFixed . round . (*) (10^3)
    floatToFixed Prec4 = Fixed4 . F4 . MkFixed . round . (*) (10^4)
    floatToFixed Prec5 = Fixed5 . F5 . MkFixed . round . (*) (10^5)
    floatToFixed Prec6 = Fixed6 . F6 . MkFixed . round . (*) (10^6)

noop :: (EvalM as ac c asset account sk) (Value as ac c)
noop = pure VVoid


-- XXX find a better way to do this
extractAddrAccount :: Value as ac c -> ac
extractAddrAccount (VAccount addr) = addr
extractAddrAccount _ = panicImpossible $ Just "extractAddrAccount"

extractAddrAsset :: Value as ac c -> as
extractAddrAsset (VAsset addr) = addr
extractAddrAsset _ = panicImpossible $ Just "extractAddrAsset"

extractAddrContract :: Value as ac c -> c
extractAddrContract (VContract addr) = addr
extractAddrContract _ = panicImpossible $ Just "extractAddrContract"

-------------------------------------------------------------------------------
-- Method Precondition Evaluation and Checking
-------------------------------------------------------------------------------

-- Evaluated preconditions
data PreconditionsV ac = PreconditionsV
  { afterV :: Maybe DateTime
  , beforeV :: Maybe DateTime
  , roleV :: Maybe (Set ac)
  }

-- | Succeeds when all preconditions are fulfilled and throws an error otherwise
-- NB: We assume that we are given a type checked AST
evalPreconditions
  :: forall as ac c asset account sk.
  (Ord as, Ord ac, Ord c, Show as, Show ac, Show c, Ledger.Addressable asset, Ledger.Addressable account)
  => Method as ac c
  -> (EvalM as ac c asset account sk) (PreconditionsV ac)
evalPreconditions m = do
  let Preconditions ps = methodPreconditions m
  PreconditionsV
    <$> sequence (evalAfter <$> List.lookup PrecAfter ps)
    <*> sequence (evalBefore <$> List.lookup PrecBefore ps)
    <*> sequence (evalRole <$> List.lookup PrecRoles ps)
  where
    evalAfter :: LExpr as ac c -> (EvalM as ac c asset account sk) DateTime
    evalAfter expr = do
      VDateTime dt <- evalLExpr expr
      pure dt

    evalBefore :: LExpr as ac c -> (EvalM as ac c asset account sk) DateTime
    evalBefore expr = do
      VDateTime dt <- evalLExpr expr
      pure dt

    evalRole :: LExpr as ac c -> (EvalM as ac c asset account sk) (Set ac)
    evalRole expr = do
      VSet vAccounts <- evalLExpr expr
      let accounts = Set.map (\(VAccount a) -> a) vAccounts
      pure accounts

checkPreconditions
  :: (Ord as, Ord ac, Ord c, Show as, Show ac, Show c, Ledger.Addressable asset, Ledger.Addressable account)
  => Method as ac c -> (EvalM as ac c asset account sk) ()
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

evalCallableMethods
  :: (Ord as, Ord ac, Ord c, Show as, Show ac, Show c, Ledger.Addressable asset, Ledger.Addressable account)
  => Contract.Contract as ac c -> (EvalM as ac c asset account sk) (Contract.CallableMethods ac)
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
hashValue :: (Show as, Show ac, Show c) => Value as ac c -> (EvalM as ac c asset account sk) ByteString
hashValue = \case
  VText msg       -> pure $ SS.toBytes msg
  VInt n         -> pure (show n)
  VFloat n       -> pure (show n)
  VFixed n       -> pure (show n)
  VBool n        -> pure (show n)
  VState n       -> pure (show n)
  VAccount a     -> notImplemented
    -- TODO: pure (rawAddr a)
  VContract a    -> notImplemented
    -- TODO: -> pure (rawAddr a)
  VAsset a       -> notImplemented
    -- TODO: -> pure (rawAddr a)
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

panicInvalidBinOp :: (Show as, Show ac, Show c) => BinOp -> Value as ac c -> Value as ac c -> a
panicInvalidBinOp op x y = panicImpossible $ Just $
  "Operator " <> show op <> " cannot be used with " <> show x <> " and " <> show y

panicInvalidUnOp :: (Show as, Show ac, Show c) => UnOp -> Value as ac c -> a
panicInvalidUnOp op x = panicImpossible $ Just $
  "Operator " <> show op <> " cannot be used with " <> show x

panicInvalidHoldingsVal :: (Show as, Show ac, Show c) => Value as ac c -> a
panicInvalidHoldingsVal v = panicImpossible $ Just $
  "Only VInt, VBool, and VFixed can be values of asset holdings. Instead, saw: "  <> show v
