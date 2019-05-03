{-# LANGUAGE TupleSections #-}

module Script.Init (
  -- createContract,
  createContractWithEvalCtx,

  createFauxContract,
) where

import           Protolude

import           Contract       (Contract)
import           Script

import qualified Contract
import qualified SafeString
import           Time           (Timestamp)
-- import qualified Transaction as TX
import qualified Key
import           Ledger         (Addressable, World)
import qualified Script.Compile as Compile
import qualified Script.Eval    as Eval
import qualified Script.Storage as Storage

-- -- | Create a contract
-- createContract
--   :: c                    -- ^ Contract Address
--   -> ac                     -- ^ Address of Evaluating node
--   -> Maybe (Eval.TransactionCtx ac)            -- ^ Maybe info relating to the transaction of the method call
--   -> sk                           -- ^ Node private key for signing
--   -> Timestamp                            -- ^ Contract timestamp
--   -> ac                     -- ^ Contract owner
--   -> World as ac c asset account                                -- ^ Initial world
--   -> Text                                 -- ^ Raw FCL code
--   -> IO (Either Text (Contract as ac c))
-- createContract contractAddr nodeAddr mtxCtx privKey cTimestamp cOwner world body = do
--   case Compile.compilePrettyErr body of
--     Left err -> pure (Left err)
--     Right Compile.CheckedScript{Compile.checkedScript = script} -> do
--       let helpers = scriptHelpers script
--       createContractWithEvalCtx (mkEvalCtx helpers) world script
--   where
--     mkEvalCtx helpers = Eval.EvalCtx
--       { currentValidator = nodeAddr
--       , currentTxCtx = mtxCtx
--       , currentCreated = cTimestamp
--       , currentDeployer = cOwner
--       , currentAddress = contractAddr
--       , currentPrivKey = privKey
--       , currentHelpers = helpers
--       }

-- | Create a contract with a supplied evaluation context
createContractWithEvalCtx
  :: (Ord as, Ord ac, Ord c, Show as, Show ac, Show c, Ledger.Addressable asset, Ledger.Addressable account, Key.Key sk)
  => Eval.EvalCtx as ac c sk -- ^ Context to evaluate the top-level definitions in
  -> World as ac c asset account      -- ^ Initial world
  -> Script as ac c     -- ^ Raw FCL code
  -> IO (Either Text (Contract as ac c))
createContractWithEvalCtx evalCtx world ast = do
  gs <- Storage.initStorage evalCtx world ast
  case Eval.transactionBlockTs <$> Eval.currentTxCtx evalCtx of
    Nothing -> pure (Left "Cannot create Contract without a transaction context.")
    Just txBlockTs -> pure . pure $ Contract.Contract
      { Contract.timestamp        = txBlockTs
      , Contract.script           = ast
      , Contract.globalStorage    = gs
      , Contract.methods          = Script.locVal <$> Script.methodNames ast
      , Contract.state            = startState
      , Contract.owner            = Eval.currentDeployer evalCtx
      , Contract.address          = Eval.currentAddress evalCtx
      }

-- | This function is used to create Contracts that explicitly _won't_ be
-- submitted to the ledger. This function creates a transaction and then derives
-- an address from the transaction. Notably, this address is _not_ guaranteed to
-- be unique, because the transaction will NOT be submitted to the network for
-- uniqueness verification. This is to be used in places like the REPL and the
-- Simulation process.
createFauxContract
  :: ac                     -- ^ Address of Evaluating node
  -> Maybe (Eval.TransactionCtx ac)            -- ^ Maybe some transaction context
  -> sk                           -- ^ Node private key for signing
  -> Timestamp                            -- ^ Contract timestamp
  -> ac                     -- ^ Contract owner
  -> World as ac c asset account                                -- ^ Initial world
  -> Text                                 -- ^ Raw FCL code
  -> IO (Either Text (Contract as ac c))
createFauxContract nodeAddr mtxCtx privKey cTimestamp cOwner world body = do
  notImplemented
  -- do
  -- let contractHdr = TX.TxContract $ TX.CreateContract (SafeString.fromBytes' $ toS body)
  -- contractAddr <- TX.transactionToAddress <$> TX.newTransaction nodeAddr privKey contractHdr
  -- createContract contractAddr nodeAddr mtxCtx privKey cTimestamp cOwner world body
