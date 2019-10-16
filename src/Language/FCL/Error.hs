{-|

Script evaluation errors.

-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Language.FCL.Error (
  EvalFail(..),
  NotCallableReason(..)
) where

import Protolude hiding (Overflow, Underflow, DivideByZero)

import Data.Aeson (ToJSON(..), sumEncoding, genericToJSON, SumEncoding(..), defaultOptions)
import Data.Serialize (Serialize)
import Test.QuickCheck
import Generic.Random

import Language.FCL.Address
import Language.FCL.Pretty hiding ((<>))
import Language.FCL.AST

-- | Scripts either run to completion or fail with a named error.
data EvalFail
  = AssetIntegrity Text                 -- ^ Asset does not support operation over it
  | AddressIntegrity Text               -- ^ Address does not exist
  | ContractIntegrity Text              -- ^ Contract does not exist
  | AccountIntegrity Text               -- ^ Account does not exist
  | InvalidMethodName Name              -- ^ Name lookup failure
  | WorkflowTerminated                  -- ^ Execution is in terminal state.
  | MethodArityError Name Int Int       -- ^ Call a function with the wrong # of args
  | Overflow                            -- ^ Overflow
  | Underflow                           -- ^ Underflow
  | DivideByZero                        -- ^ Division by zero
  | Impossible Text                     -- ^ Internal error
  | NoSuchPrimOp Name                   -- ^ Prim op name lookup fail
  | LookupFail Text                     -- ^ Foldable/Traversable type lookup fail
  | ModifyFail Text                     -- ^ Map modify fail
  | CallPrimOpFail Loc (Maybe Value) Text -- ^ Prim op call failed
  | NoTransactionContext Loc Text       -- ^ Asked for a bit of transaction context without a transaction context
  | PatternMatchFailure Value Loc       -- ^ No matching pattern
  | NotCallable Name NotCallableReason
  deriving (Eq, Show, Generic, Serialize)

instance Arbitrary EvalFail where
  arbitrary = genericArbitraryU

instance Pretty EvalFail where
  ppr e = case e of
    AssetIntegrity err                 -> "Asset integrity error:" <+> ppr err
    AddressIntegrity err               -> "Address integrity error:" <+> ppr err
    ContractIntegrity err              -> "Contract integrity error:" <+> ppr err
    AccountIntegrity err               -> "Account integrity error:" <+> ppr err
    InvalidMethodName nm               -> "Invalid method name:" <+> ppr nm
    WorkflowTerminated                 -> "Error: the workflow has been terminated and is no longer live."
    MethodArityError nm expected given -> "Method arity error:"
                                          <$$+> "expected" <+> ppr expected
                                            <+> "args, given " <+> ppr given
    Overflow                           -> "Overflow"
    Underflow                          -> "Underflow"
    DivideByZero                       -> "DivideByZero"
    Impossible err                     -> "Internal error:" <+> ppr err
    NoSuchPrimOp nm                    -> "No such primop:" <+> ppr nm
    LookupFail k                       -> "Lookup fail with key:" <+> ppr k
    ModifyFail k                       -> "Modify map failure, no value with key:" <+> ppr k
    CallPrimOpFail loc mval msg        -> "Evaluation error at" <+> ppr loc <> ":"
                                       <$$+> case mval of
                                               Nothing -> ppr msg
                                               Just v  -> "Invalid value" <+> ppr v
                                                          <$$+> ppr msg
    NoTransactionContext loc msg       -> "Invalid transaction context info request at" <+> ppr loc <> ":"
                                       <$$+> ppr msg

    PatternMatchFailure val loc ->
      "No matching pattern for value" <+> sqppr val <+> "at" <+> ppr loc
    NotCallable methodName reason -> "Method" <+> ppr methodName <+> "is not callable:"
      <$$+> ppr reason


instance Pretty NotCallableReason where
  ppr = \case
    ErrWorkflowState w1 w2 ->
      "Workflow state precondition error:"
        <$$+> "Required state is" <+> ppr w1 <+> ", but actual is" <+> ppr w2
    ErrPrecAfter dtExpected dtActual ->
      "Method only callable after: " <+> ppr (VDateTime dtExpected)
        <+> ". Actual date-time:" <+> ppr (VDateTime dtActual)
    ErrPrecBefore dtExpected dtActual ->
      "Method only callable before: " <+> ppr (VDateTime dtExpected)
        <+> ". Actual date-time:" <+> ppr (VDateTime dtActual)
    ErrPrecCaller setAccExpected accActual ->
      "Transaction issuer not authorised."
          <$$+> vcat
          [ "Transaction issuer: " <+> ppr accActual
          , "Authorized accounts: " <+> setOf setAccExpected
          ]

-- | Method Precondition errors, fields of the form  <expected> <actual>`.
data NotCallableReason
  = ErrWorkflowState -- ^ Workflow state precondition error
    { expectedWS :: WorkflowState, actualWS :: WorkflowState }
  | ErrPrecAfter -- ^ Method only callable after
    { expectedAfter :: DateTime, actualAfter :: DateTime }
  | ErrPrecBefore -- ^ Method only callable before
    { expectedBefore :: DateTime, actualBefore :: DateTime }
  | ErrPrecCaller -- ^ Transaction issuer not authorised
    { expectedCaller :: Set (Address AAccount), actualCaller :: Address AAccount }
  deriving (Eq, Show, Generic, Serialize)

instance ToJSON NotCallableReason where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance Arbitrary NotCallableReason where
  arbitrary = genericArbitraryU
