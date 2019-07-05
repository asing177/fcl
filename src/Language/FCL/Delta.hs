{-|

Ledger deltas , atomic operations on the ledger state as a result of block evaluation.

-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.FCL.Delta (
  -- ** Delta operations
  Delta(..),
  AssetOp(..),
  DeltaCtx(..),

  -- ** Printing
  dumpDeltas,
) where

import Protolude hiding ((<>))

import Language.FCL.Asset (Balance)
import Language.FCL.AST
import Language.FCL.Address
import Language.FCL.Pretty
import Language.FCL.Time
import Language.FCL.Error (EvalFail(..))

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
  | ModifyAsset DeltaCtx AssetOp           -- ^ Modify an asset
  | ModifyState WorkflowState     -- ^ Set a new workflow state

  -- Transactions
  | Atomic Delta Delta       -- ^ Combine two operations as atomic (swap)
  | Terminate                -- ^ Terminate the contract

  -- Evaluation failures
  | Failure EvalFail
  deriving (Eq, Show)

data DeltaCtx
  = DeltaCtx
    { dctxMethodNm :: Maybe Name
    }
  deriving (Eq, Show)

instance Pretty DeltaCtx where
  ppr (DeltaCtx (Just dctxMethodNm)) = ppr dctxMethodNm
  ppr (DeltaCtx Nothing) = ppr ("No method found" :: Text)

data AssetOp
  = TransferTo {
      asset    :: Address AAsset    -- ^ Asset to transfer
    , amount   :: Balance           -- ^ Amount
    , holder   :: Address AAccount  -- ^ Holder of the asset
    , contract :: Address AContract -- ^ Contract address
   } -- ^ Transfer holdings to contract

  | TransferFrom {
      asset    :: Address AAsset    -- ^ Asset to transfer
    , amount   :: Balance             -- ^ Amount
    , to       :: Address AAccount  -- ^ Receipient
    , contract :: Address AContract -- ^ Contract address
  } -- ^ Transfer holdings from contract to account

  | TransferHoldings {
      from   :: Address AAccount  -- ^ Sender
    , asset  :: Address AAsset    -- ^ Asset to transfer
    , amount :: Balance             -- ^ Amount
    , to     :: Address AAccount  -- ^ Receipient
  } -- ^ Transfer holdings from account to account

  | Revert {
      asset   :: Address AAsset  -- ^ Asset to transfer-
  } -- ^ Revert holdings to issuer

  deriving (Eq, Ord, Show, Generic)

-------------------------------------------------------------------------------
-- Printing
-------------------------------------------------------------------------------


instance Pretty [Delta] where
  ppr = blockWith vcat '[' ']' . fmap ppr

-- | Pretty print delta
instance Pretty Delta where
  ppr = \case
    ModifyGlobal nm val -> "global" <+> ppr nm <+> "=" <+> ppr val
    ModifyState st -> "state" <+> "=" <+> ppr st

    ModifyAsset ctx op -> do
      case ctx of
        DeltaCtx Nothing -> pprOp op
        DeltaCtx (Just nm) -> "calling method" <+> "\"" <> ppr (unName nm) <> "\"" <+> "causes" <+> pprOp op
        where
          pprOp op = case op of
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
