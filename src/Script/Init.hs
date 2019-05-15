{-# LANGUAGE TupleSections #-}

module Script.Init (
  createContract,
  createContractWithEvalCtx,

  createFauxContract,
) where

import Protolude

import Script
import Script.Pretty (prettyPrint)
import Address (Address, AAccount, AContract)
import Contract (Contract)

import Key (PrivateKey)
import Time (Timestamp)
import qualified Contract
import qualified SafeString
import qualified Transaction as TX
import qualified Script.Storage as Storage
import Ledger (World)
import qualified Script.Eval as Eval

-- | Create a contract
createContract
  :: Address AContract                    -- ^ Contract Address
  -> Address AAccount                     -- ^ Address of Evaluating node
  -> Maybe Eval.TransactionCtx            -- ^ Maybe info relating to the transaction of the method call
  -> PrivateKey                           -- ^ Node private key for signing
  -> Timestamp                            -- ^ Contract timestamp
  -> Address AAccount                     -- ^ Contract owner
  -> World                                -- ^ Initial world
  -> Script                               -- ^ FCL Script
  -> IO (Either Text Contract)
createContract contractAddr nodeAddr mtxCtx privKey cTimestamp cOwner world script
  = createContractWithEvalCtx evalCtx world script
  where
    evalCtx = Eval.EvalCtx
      { currentValidator = nodeAddr
      , currentTxCtx = mtxCtx
      , currentCreated = cTimestamp
      , currentDeployer = cOwner
      , currentAddress = contractAddr
      , currentPrivKey = privKey
      , currentHelpers = scriptHelpers script
      }

-- | Create a contract with a supplied evaluation context
createContractWithEvalCtx
  :: Eval.EvalCtx -- ^ Context to evaluate the top-level definitions in
  -> World      -- ^ Initial world
  -> Script     -- ^ FCL script
  -> IO (Either Text Contract)
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
  :: Address AAccount                     -- ^ Address of Evaluating node
  -> Maybe Eval.TransactionCtx            -- ^ Maybe some transaction context
  -> PrivateKey                           -- ^ Node private key for signing
  -> Timestamp                            -- ^ Contract timestamp
  -> Address AAccount                     -- ^ Contract owner
  -> World                                -- ^ Initial world
  -> Script                               -- ^ FCL script
  -> IO (Either Text Contract)
createFauxContract nodeAddr mtxCtx privKey cTimestamp cOwner world script = do
  let contractHdr = TX.TxContract $ TX.CreateContract (SafeString.fromBytes' $ toS $ prettyPrint script)
  contractAddr <- TX.transactionToAddress <$> TX.newTransaction nodeAddr privKey contractHdr
  createContract contractAddr nodeAddr mtxCtx privKey cTimestamp cOwner world script
