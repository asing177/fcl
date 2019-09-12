{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
module SafeWorkflow.DSL where

import Protolude hiding (Type, sequence, option)

import qualified Data.ByteString as BS

import Numeric.Lossless.Decimal (Decimal(..))

import Language.FCL.AST
import Language.FCL.Address (Address(..))
import Language.FCL.SafeWorkflow.Builder

import qualified Language.FCL.SafeWorkflow.Builder as SW

import SafeWorkflow.TH

-- TODO: remove this
import Language.FCL.Pretty

xLTFive :: Expr
xLTFive = EBinOp (noLoc Lesser)
  (noLoc $ EVar $ noLoc $ "x")
  (noLoc $ ELit $ noLoc $ LNum $ Decimal 0 5)

xEQZero :: Expr
xEQZero = EBinOp (noLoc Equal)
  (noLoc $ EVar $ noLoc $ "x")
  (noLoc $ ELit $ noLoc $ LNum $ Decimal 0 0)

xAssign :: Integer -> Expr
xAssign n = EAssign ["x"]
  (noLoc $ ELit $ noLoc $ LNum $ Decimal 0 n)

mkArg :: Type -> Name -> Arg
mkArg ty name = Arg ty (noLoc name)

account :: Word8 -> Expr
account = ELit . noLoc . LAccount . Address . BS.singleton

decimal :: Integer -> Type
decimal = TNum . NPDecimalPlaces

int :: Type
int = TNum nPInt

simpleWhiteBoardExample3 :: SW.Builder ()
simpleWhiteBoardExample3 = do
  parallel 1
    "split" (xAssign 10)
    "join"  (xAssign 20)
    [ ("lhsIn", "lhsOut")
    , ("rhsIn", "rhsOut")
    ]
  finish 1 "t1" $ xAssign 1
  conditional 2 xLTFive
  finish 8 "t2" $ xAssign 2
  stayOrContinue 9 xEQZero
  finish 11 "t2" $ xAssign 3
  finish 12 "t2" $ xAssign 4

conditionalInConditionalLeft :: SW.Builder ()
conditionalInConditionalLeft = do
  conditional 1 xLTFive
  conditional 1 xEQZero
  finish 2 "t1" $ xAssign 1
  finish 3 "t1" $ xAssign 2
  finish 4 "t1" $ xAssign 3

conditionalInConditionalRight :: SW.Builder ()
conditionalInConditionalRight = do
  conditional 1 xLTFive
  finish 1 "t1" $ xAssign 1
  conditional 2 xEQZero
  finish 4 "t1" $ xAssign 2
  finish 5 "t1" $ xAssign 3

seqInConditionalLeft :: SW.Builder ()
seqInConditionalLeft = do
  conditional 1 xLTFive
  sequence 1 "inbetween"
  finish 11 "t1" $ xAssign 1
  finish 12 "t2" $ xAssign 2
  finish 2  "t1" $ xAssign 3

seqInConditionalRight :: SW.Builder ()
seqInConditionalRight = do
  conditional 1 xLTFive
  sequence 2 "inbetween"
  finish 21 "t1" $ xAssign 1
  finish 22 "t2" $ xAssign 2
  finish 1  "t1" $ xAssign 3

simpleOption :: SW.Builder ()
simpleOption = do
  addGlobalWithDefault TAccount "alice" (account 100)
  addGlobalWithDefault TAccount "bob"   (account 101)
  option 1
  finish 1 "t1" $ xAssign 1
  finish 2 "t2" $ xAssign 2
  addPrecondition "t1" PrecRoles (role "alice")
  addPrecondition "t2" PrecRoles (role "bob")
  addArgs "t1" [mkArg TBool "b"]

-- FIXME: you shouldn't be able to do undeterministic branching
-- inside a deterministic IF condition
-- TODO: disallow this
optionInConditionalLeft :: SW.Builder ()
optionInConditionalLeft = do
  conditional 1 xLTFive
  option 1
  finish 11 "t1" $ xAssign 1
  finish 12 "t2" $ xAssign 2
  finish 2  "t1" $ xAssign 3

loanContract :: SW.Builder ()
loanContract = do
  addGlobalSimple (decimal 2)          "principle"
  addGlobalSimple (decimal 2)          "interest_rate"
  addGlobalSimple (TAsset $ decimal 2) "currency"
  addGlobalSimple TAccount             "borrower"
  addGlobalSimple TAccount             "lender"
  addGlobalSimple TText                "loan_contract"

  sequence 1 "negotiate_terms"
  finish 2 "propose_contract" [fcl|
    {borrower = borrower_arg;
    lender = lender_arg;
    principle = principle_arg;
    currency = currency_arg;
    interest_rate = interest_rate_arg;}
    |]

  -- NOTE: no condition needed, since the branching is non-deterministic
  loopOrContinue 3 ENoOp "make_decision"

  finish 6 "propose_terms" [fcl|
    {loan_contract = loan_contract_arg;}
    |]

  finish 8 "revise" ENoOp
  option 7
  finish 12 "reject" ENoOp
  sequence 11 "signed"
  finish 15 "sign" ENoOp
  sequence 16 "contract_active"

  finish 19 "loan_start" [fcl|
    {transferHoldings(lender, currency, principle, borrower);}
    |]

  -- NOTE: no condition needed, since the branching is non-deterministic
  stayOrContinue 20 ENoOp

  finish 22 "payback" [fcl|
    {transferHoldings(lender, currency, principle, borrower);}
    |]

  finish 23 "pay_interest" [fcl|
    {interest_payment = round(2,principle * interest_rate);
    transferHoldings(borrower, currency, interest_payment, lender);}
    |]

  addRole "propose_terms" "lender"
  addRole "sign"          "borrower"
  addRole "revise"        "borrower"
  addRole "reject"        "borrower"
  addRole "loan_start"    "lender"
  addRole "loan_interest" "borrower"
  addRole "payback"       "borrower"

  addArgs "propose_contract"
    [ mkArg (decimal 2) "principle_arg"
    , mkArg (TAsset $ decimal 2) "currency_arg"
    , mkArg TAccount "borrower_arg"
    , mkArg TAccount "lender_arg"
    , mkArg (decimal 2) "interest_rate_arg"
    ]

  addArgs "propose_terms"
    [ mkArg TText "loan_contract_arg"
    ]

amendment :: SW.Builder ()
amendment = do
  addGlobalSimple TAccount "alice"
  addGlobalSimple TAccount "bob"
  addGlobalSimple int      "valueAlice"
  addGlobalSimple int      "valueBob"
  addGlobalSimple int      "total"
  addGlobalSimple int      "proposedNewTotal"
  addGlobalSimple TAccount "counterparty"

  sequence 1 "totalCalculated"
  parallel 2
    "init" [fcl|
      {alice = a;
      bob = b;}
      |]
    "calculateTotal" [fcl|
      {total = valueAlice + valueBob;}
      |]
    [ ("todoAlice", "doneAlice")
    , ("todoBob",   "doneBob")
    ]

  -- NOTE: nondeterministic loop
  nondetStayOrContinue 3

  sequence 11 "agreeAmendment"
  conditional 13 [fcl| {sender() == alice} |]
  stayOrContinue 16 [fcl| {sender() == bob} |]

  finish 1 "setValueAlice" [fcl| {valueAlice = val;} |]
  finish 2 "setValueBob"   [fcl| {valueBob   = val;} |]

  finish 15 "proposeNewTotal" [fcl|
    {counterparty = bob;}
    |]
  finish 17 "proposeNewTotal" [fcl|
    {counterparty = alice;}
    |]
  finish 18 "proposeNewTotal" ENoOp -- good

  finish 14 "agreeAmendment"  [fcl|
    {if (agrees) {
      total = proposedNewTotal;
    };}
    |]

  finish 10 "end" ENoOp -- good

  addBranchIndependentCode "proposeNewTotal" [fcl|
    {proposedNewTotal = newTotal;}
    |]

  addRole "setValueAlice"     "alice"
  addRole "setValueBob"       "bob"
  -- NOTE: two roles for the same method
  addRole "proposeNewTotal"   "alice"
  addRole "proposeNewTotal"   "bob"
  addRole "agreeAmendment"    "counterparty"

  addArgs "init"
    [ mkArg TAccount "a"
    , mkArg TAccount "b"
    ]
  addArgs "setValueAlice"
    [ mkArg int "val"
    ]
  addArgs "setValueBob"
    [ mkArg int "val"
    ]
  addArgs "proposeNewTotal"
    [ mkArg int "newTotal"
    ]
  addArgs "agreeAmendment"
    [ mkArg TBool "agrees"
    ]
