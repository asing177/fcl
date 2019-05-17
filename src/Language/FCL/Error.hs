{-# LANGUAGE DataKinds #-}
{-|

Script evaluation errors.

--}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Language.FCL.Error (
  EvalFail(..),
) where

import Protolude hiding (Overflow, Underflow, DivideByZero)

import Data.Serialize (Serialize)

import Language.FCL.Address
import Language.FCL.Pretty hiding ((<>))
import Language.FCL.Contract (InvalidMethodName)
import Language.FCL.AST

-- | Scripts either run to completion or fail with a named error.
data EvalFail
  = AssetIntegrity Text                 -- ^ Asset does not support operation over it
  | AddressIntegrity Text               -- ^ Address does not exist
  | ContractIntegrity Text              -- ^ Contract does not exist
  | AccountIntegrity Text               -- ^ Account does not exist
  | InvalidMethodName InvalidMethodName -- ^ Name lookup failure
  | WorkflowTerminated                  -- ^ Execution is in terminal state.
  | MethodArityError Name Int Int       -- ^ Call a function with the wrong # of args
  | Overflow                            -- ^ Overflow
  | Underflow                           -- ^ Underflow
  | DivideByZero                        -- ^ Division by zero
  | StatePreconditionError WorkflowState WorkflowState -- ^ Invalid workflow state entry
  | Impossible Text                     -- ^ Internal error
  | HugeInteger Text                    -- ^ SafeInteger bounds exceeded
  | HugeString Text                     -- ^ SafeString bounds exceeded
  | NoSuchPrimOp Name                   -- ^ Prim op name lookup fail
  | LookupFail Text                     -- ^ Foldable/Traversable type lookup fail
  | ModifyFail Text                     -- ^ Map modify fail
  | CallPrimOpFail Loc (Maybe Value) Text -- ^ Prim op call failed
  | NoTransactionContext Loc Text       -- ^ Asked for a bit of transaction context without a transaction context
  -- Precondition errors, all of the form `PrecNotSatX Method <expected> <actual>`
  | PrecNotSatAfter Method DateTime DateTime
  | PrecNotSatBefore Method DateTime DateTime
  | PrecNotSatCaller Method (Set (Address AAccount)) (Address AAccount)
  deriving (Eq, Show, Generic, Serialize)

instance Pretty EvalFail where
  ppr e = case e of
    AssetIntegrity err                 -> "Asset integrity error:" <+> ppr err
    AddressIntegrity err               -> "Address integrity error:" <+> ppr err
    ContractIntegrity err              -> "Contract integrity error:" <+> ppr err
    AccountIntegrity err               -> "Account integrity error:" <+> ppr err
    InvalidMethodName err              -> "Invalid method name:" -- XXX pretty print invalid method name
    WorkflowTerminated                 -> "Error: the workflow has been terminated and is no longer live."
    MethodArityError nm expected given -> "Method arity error:"
                                          <$$+> "expected" <+> ppr expected
                                            <+> "args, given " <+> ppr given
    Overflow                           -> "Overflow"
    Underflow                          -> "Underflow"
    DivideByZero                       -> "DivideByZero"
    StatePreconditionError w1 w2       -> "Workflow state precondition error:"
                                          <$$+> "Required state is" <+> ppr w1
                                            <+> ", but actual is" <+> ppr w2
    Impossible err                     -> "Internal error:" <+> ppr err
    HugeInteger err                    -> "SafeInteger bounds exceeded:" <+> ppr err
    HugeString err                     -> "SafeString bounds exceeded:" <+> ppr err
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
    PrecNotSatAfter m dtExpected dtActual ->
      "Temporal precondition for calling method" <+> ppr (methodName m) <+> "not satisfied."
      <$$+> "Method only callable after: " <+> ppr (VDateTime dtExpected)
        <+> ". Actual date-time:" <+> ppr (VDateTime dtActual)
    PrecNotSatBefore m dtExpected dtActual ->
      "Temporal precondition for calling method" <+> ppr (methodName m) <+> "not satisfied."
      <$$+> "Method only callable before: " <+> ppr (VDateTime dtExpected)
        <+> ". Actual date-time:" <+> ppr (VDateTime dtActual)
    PrecNotSatCaller m setAccExpected accActual ->
      "Unauthorized to call method" <+> sqppr (methodName m) <> "."
          <$$+> vcat
          [ "Transaction issuer: " <+> ppr accActual
          , "Authorized accounts: " <+> setOf setAccExpected
          ]
