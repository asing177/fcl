{-|

Ledger deltas , atomic operations on the ledger state as a result of block evaluation.

-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Delta (
  -- ** Delta operations
  Delta(..),
  AssetOp(..),

  -- ** Printing
  dumpDeltas,
) where

import Protolude hiding ((<>))

import Asset (Balance)
import Script
import Script.Pretty
import Script.Error (EvalFail(..))

-------------------------------------------------------------------------------
-- Delta
-------------------------------------------------------------------------------

-- | A delta is a side-effect induced by running a method on a script, that
-- alters some aspect of the ledger world state. When a block is mined, all
-- deltas are computed for all transactions and applied to the end-result
-- world-state of the block.
data Delta
  -- Contract state changes
  = ModifyGlobal Name Value       -- ^ Modify a contract state variable
  | ModifyAsset AssetOp           -- ^ Modify an asset
  | ModifyState WorkflowState     -- ^ Set a new workflow state

  -- Transactions
  | Atomic Delta Delta       -- ^ Combine two operations as atomic (swap)
  | Terminate                -- ^ Terminate the contract

  -- Evaluation failures
  | Failure EvalFail
  deriving (Eq, Show, Generic)

data AssetOp
  = TransferTo {
      asset    :: Addr    -- ^ Asset to transfer
    , amount   :: Balance           -- ^ Amount
    , holder   :: Addr  -- ^ Holder of the asset
    , contract :: Addr -- ^ Contract address
   } -- ^ Transfer holdings to contract

  | TransferFrom {
      asset    :: Addr    -- ^ Asset to transfer
    , amount   :: Balance             -- ^ Amount
    , to       :: Addr  -- ^ Receipient
    , contract :: Addr -- ^ Contract address
  } -- ^ Transfer holdings from contract to account

  | TransferHoldings {
      from   :: Addr  -- ^ Sender
    , asset  :: Addr    -- ^ Asset to transfer
    , amount :: Balance             -- ^ Amount
    , to     :: Addr  -- ^ Receipient
  } -- ^ Transfer holdings from account to account

  | Revert {
      asset   :: Addr  -- ^ Asset to transfer-
  } -- ^ Revert holdings to issuer

  deriving (Eq, Ord, Show, Generic)

-------------------------------------------------------------------------------
-- Printing
-------------------------------------------------------------------------------

-- | Pretty print delta
instance Pretty Delta where
  ppr = \case
    ModifyGlobal nm val -> "global" <+> ppr nm <+> "=" <+> ppr val
    ModifyState st -> "state" <+> "=" <+> ppr st

    ModifyAsset op -> case op of
      TransferTo asset amt holder contract       ->
        "transferTo" <+> ppr asset
      TransferFrom asset amt holder contract     ->
        "transferFrom" <+> ppr asset
      TransferHoldings asset amt holder contract ->
        "transferHoldings" <+> ppr asset
      Revert asset                               ->
        "revert" <+> ppr asset

    Atomic a1 a2 -> "atomic" <+> "{" <+> ppr a1 <+> "," <+> ppr a2 <+> "}"
    Terminate -> "terminate"
    Failure mode -> "failure" <> parens (ppr (show mode :: Text))

-- | Pretty print delta list
dumpDeltas :: [Delta] -> Doc
dumpDeltas deltas = indent 8 $ vcat (fmap ppr deltas)
