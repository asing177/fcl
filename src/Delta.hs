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
data Delta as ac c
  -- Contract state changes
  = ModifyGlobal Name (Value as ac c)       -- ^ Modify a contract state variable
  | ModifyAsset (AssetOp as ac c)           -- ^ Modify an asset
  | ModifyState WorkflowState     -- ^ Set a new workflow state

  -- Transactions
  | Atomic (Delta as ac c) (Delta as ac c)       -- ^ Combine two operations as atomic (swap)
  | Terminate                -- ^ Terminate the contract

  -- Evaluation failures
  | Failure (EvalFail as ac c)
  deriving (Eq, Show, Generic, NFData)

data AssetOp as ac c
  = TransferTo {
      asset    :: as    -- ^ Asset to transfer
    , amount   :: Int64             -- ^ Amount
    , holder   :: ac  -- ^ Holder of the asset
    , contract :: c -- ^ Contract address
   } -- ^ Transfer holdings to contract

  | TransferFrom {
      asset    :: as    -- ^ Asset to transfer
    , amount   :: Int64             -- ^ Amount
    , to       :: ac  -- ^ Receipient
    , contract :: c  -- ^ Contract address
  } -- ^ Transfer holdings from contract to account

  | TransferHoldings {
      from   :: ac  -- ^ Sender
    , asset  :: as    -- ^ Asset to transfer
    , amount :: Int64             -- ^ Amount
    , to     :: ac  -- ^ Receipient
  } -- ^ Transfer holdings from account to account

  | Revert {
      asset   :: as  -- ^ Asset to transfer-
  } -- ^ Revert holdings to issuer

  deriving (Eq, Ord, Show, Generic, NFData)

-------------------------------------------------------------------------------
-- Printing
-------------------------------------------------------------------------------

-- | Pretty print delta
instance (Show as, Show ac, Show c, Pretty as, Pretty ac, Pretty c) => Pretty (Delta as ac c) where
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
dumpDeltas :: (Show as, Show ac, Show c, Pretty as, Pretty ac, Pretty c) => [Delta as ac c] -> Doc
dumpDeltas deltas = indent 8 $ vcat (fmap ppr deltas)
