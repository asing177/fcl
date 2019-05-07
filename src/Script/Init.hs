{-# LANGUAGE TupleSections #-}

module Script.Init (
  createContract,
  createContractWithEvalCtx,
) where

import           Protolude

import           Contract       (Contract)
import           Script

import qualified Contract
import qualified SafeString
import qualified Script.Parser  as Parser
import           Time           (Timestamp)
-- import qualified Transaction as TX
import qualified Key
import           Ledger         (Addressable, WorldOps)
import qualified Script.Compile as Compile
import qualified Script.Eval    as Eval
import           Script.Pretty
import qualified Script.Storage as Storage

-- | Create a contract
createContract
  :: (Show as, Show ac, Show c, Ord as, Ord ac, Ord c, Ledger.Addressable asset as, Ledger.Addressable account ac, WorldOps w, Key.Key sk, Pretty as, Pretty ac, Pretty c)
  => Proxy (asset, account)
  -> c                    -- ^ Contract Address
  -> ac                     -- ^ Address of Evaluating node
  -> Maybe (Eval.TransactionCtx ac)            -- ^ Maybe info relating to the transaction of the method call
  -> sk                           -- ^ Node private key for signing
  -> Timestamp                            -- ^ Contract timestamp
  -> ac                     -- ^ Contract owner
  -> w                                -- ^ Initial world
  -> Text                                 -- ^ Raw FCL code
  -> ExceptT Text (ReaderT (Parser.AddrParsers as ac c) IO) (Contract as ac c)
createContract proxy contractAddr nodeAddr mtxCtx privKey cTimestamp cOwner world body = do
  addrParsers <- ask
  case (flip runReader) addrParsers $ runExceptT (Compile.compilePrettyErr body) of
    Left err -> throwE err
    Right Compile.CheckedScript{Compile.checkedScript = script} -> do
      let helpers = scriptHelpers script
      createContractWithEvalCtx proxy (mkEvalCtx helpers) world script
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
  :: (Ord as, Ord ac, Ord c, Show as, Show ac, Show c, Ledger.Addressable asset as, Ledger.Addressable account ac, Ledger.WorldOps w, Key.Key sk)
  => Proxy (asset, account)
  -> Eval.EvalCtx as ac c sk -- ^ Context to evaluate the top-level definitions in
  -> w     -- ^ Initial world
  -> Script as ac c     -- ^ Raw FCL code
  -> ExceptT Text (ReaderT (Parser.AddrParsers as ac c) IO) (Contract as ac c)
createContractWithEvalCtx proxy evalCtx world ast = do
  gs <- liftIO $ Storage.initStorage proxy evalCtx world ast
  case Eval.transactionBlockTs <$> Eval.currentTxCtx evalCtx of
    Nothing -> throwE "Cannot create Contract without a transaction context."
    Just txBlockTs -> pure $ Contract.Contract
      { Contract.timestamp        = txBlockTs
      , Contract.script           = ast
      , Contract.globalStorage    = gs
      , Contract.methods          = Script.locVal <$> Script.methodNames ast
      , Contract.state            = startState
      , Contract.owner            = Eval.currentDeployer evalCtx
      , Contract.address          = Eval.currentAddress evalCtx
      }
