{-# LANGUAGE TupleSections #-}

module Language.FCL.Init (
  createContract,
  createContractWithEvalCtx,

  createFauxContract,
) where

import Protolude

import Language.FCL.AST
import Language.FCL.Address (Address, AAccount, AContract)
import Contract (Contract)

import Key (PrivateKey)
import Language.FCL.Time (Timestamp)
import qualified Contract
import qualified SafeString
import qualified Transaction as TX
import qualified Language.FCL.Storage as Storage
import qualified Language.FCL.Compile as Compile
import Ledger (World)
import qualified Language.FCL.Eval as Eval

-- | Create a contract
createContract
  :: Address AContract                    -- ^ Contract Address
  -> Address AAccount                     -- ^ Address of Evaluating node
  -> Maybe Eval.TransactionCtx            -- ^ Maybe info relating to the transaction of the method call
  -> PrivateKey                           -- ^ Node private key for signing
  -> Timestamp                            -- ^ Contract timestamp
  -> Address AAccount                     -- ^ Contract owner
  -> World                                -- ^ Initial world
  -> Text                                 -- ^ Raw FCL code
  -> IO (Either Text Contract)
createContract contractAddr nodeAddr mtxCtx privKey cTimestamp cOwner world body = do
  case Compile.compilePrettyErr body of
    Left err -> pure (Left err)
    Right Compile.CheckedScript{Compile.checkedScript = script} -> do
      let helpers = scriptHelpers script
      createContractWithEvalCtx (mkEvalCtx helpers) world script
  where
    mkEvalCtx helpers = Eval.EvalCtx
      { currentValidator = nodeAddr
      , currentTxCtx = mtxCtx
      , currentCreated = cTimestamp
      , currentDeployer = cOwner
      , currentAddress = contractAddr
      , currentPrivKey = privKey
      , currentHelpers = helpers
      }

-- | Create a contract with a supplied evaluation context
createContractWithEvalCtx
  :: Eval.EvalCtx -- ^ Context to evaluate the top-level definitions in
  -> World      -- ^ Initial world
  -> Script     -- ^ Raw FCL code
  -> IO (Either Text Contract)
createContractWithEvalCtx evalCtx world ast = do
  gs <- Storage.initStorage evalCtx world ast
  case Eval.transactionBlockTs <$> Eval.currentTxCtx evalCtx of
    Nothing -> pure (Left "Cannot create Contract without a transaction context.")
    Just txBlockTs -> pure . pure $ Contract.Contract
      { Contract.timestamp        = txBlockTs
      , Contract.script           = ast
      , Contract.globalStorage    = gs
      , Contract.methods          = Language.FCL.locVal <$> Language.FCL.methodNames ast
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
  -> Text                                 -- ^ Raw FCL code
  -> IO (Either Text Contract)
createFauxContract nodeAddr mtxCtx privKey cTimestamp cOwner world body = do
  let contractHdr = TX.TxContract $ TX.CreateContract (SafeString.fromBytes' $ toS body)
  contractAddr <- TX.transactionToAddress <$> TX.newTransaction nodeAddr privKey contractHdr
  createContract contractAddr nodeAddr mtxCtx privKey cTimestamp cOwner world body
