{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}

module Language.FCL.Init (
  createContract,
  createContractWithEvalCtx,
) where

import Protolude

import Language.FCL.AST
import Language.FCL.Address

import Language.FCL.Key (PrivateKey)
import Language.FCL.Time (Timestamp)
import Language.FCL.Contract as Contract
import qualified Language.FCL.Compile as Compile
import qualified Language.FCL.Eval as Eval
import Language.FCL.World as World

-- | Create a contract
createContract
  :: World world
  => Address AContract                    -- ^ Contract Address
  -> Address AAccount                     -- ^ Address of Evaluating node
  -> Maybe Eval.TransactionCtx            -- ^ Maybe info relating to the transaction of the method call
  -> PrivateKey                           -- ^ Node private key for signing
  -> Timestamp                            -- ^ Contract timestamp
  -> Address AAccount                     -- ^ Contract owner
  -> world                                -- ^ Initial world
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
  :: World world
  => Eval.EvalCtx -- ^ Context to evaluate the top-level definitions in
  -> world      -- ^ Initial world
  -> Script     -- ^ Raw FCL code
  -> IO (Either Text Contract)
createContractWithEvalCtx evalCtx world ast = do
  gs <- Eval.initStorage evalCtx world ast
  case Eval.transactionBlockTs <$> Eval.currentTxCtx evalCtx of
    Nothing -> pure (Left "Cannot create Contract without a transaction context.")
    Just txBlockTs -> pure . pure $ Contract
      { Contract.timestamp        = txBlockTs
      , Contract.script           = ast
      , Contract.globalStorage    = gs
      , Contract.methods          = locVal <$> methodNames ast
      , Contract.state            = startState
      , Contract.owner            = Eval.currentDeployer evalCtx
      , Contract.address          = Eval.currentAddress evalCtx
      }
