{-|

Contract datatypes, signing and operations.

-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DataKinds #-}

module Language.FCL.Contract (
  -- ** Types
  Contract(..),

  callableMethods,
  PermittedCallers(..),
  CallableMethods(..),
  CallableMethod(..),
  NotCallableMethod(..),

  -- ** Validation
  validateContract,

  -- ** Querying contract state
  lookupVarGlobalStorage,
  lookupContractMethod,

  -- ** Signing
  signContract,
) where

import Protolude hiding (state, Type)

import Language.FCL.Time (Timestamp)
import Language.FCL.Address
import Language.FCL.Storage (GlobalStorage)
import Control.Monad

import qualified Language.FCL.Key as Key
import qualified Language.FCL.Storage as Storage
import qualified Language.FCL.Utils as Utils

import Language.FCL.AST
import Language.FCL.Error (NotCallableReason(..))
import qualified Language.FCL.Pretty as Pretty
import qualified Language.FCL.Parser as Parser
import qualified Language.FCL.Typecheck as Typecheck

import Crypto.Random.Types (MonadRandom(..))
import Data.Serialize (Serialize)
import qualified Data.Map.Strict as Map
import qualified Data.Binary as B

import Data.Aeson (ToJSON(..), (.=), (.:), sumEncoding, genericToJSON, SumEncoding(..), defaultOptions)
import Data.Aeson.Types (typeMismatch)
import qualified Data.Aeson as A

import Test.QuickCheck
import Generic.Random

-------------------------------------------------------------------------------
-- Contracts
-------------------------------------------------------------------------------

-- | A contract is a 4-tuple of the address of publisher, timestamp of
-- deployment time, script source, and it's initial storage hash.
data Contract = Contract
  { timestamp        :: Timestamp             -- ^ Timestamp of issuance
  , script           :: Script                -- ^ Underlying contract logic
  , globalStorage    :: GlobalStorage         -- ^ Initial state of the contract
  , methods          :: [Name]                -- ^ Public methods
  , state            :: WorkflowState         -- ^ State of Contract
  , owner            :: Address AAccount      -- ^ Creator of the contract
  , address          :: Address AContract     -- ^ Contract Address, derived during creation
  } deriving (Show, Generic, Serialize)

instance A.ToJSON Contract where
  toJSON Contract{..} = A.object
    [ "timestamp"     .= timestamp
    , "script"        .= Pretty.prettyPrint script
    , "storage"       .= globalStorage
    , "methods"       .= methods
    , "state"         .= state
    , "owner"         .= owner
    , "address"       .= address
    ]

instance A.FromJSON Contract where
  parseJSON = \case
      A.Object v ->
        Contract
          <$> v .: "timestamp"
          <*> (parseScriptJSON =<< v .: "script")
          <*> v .: "storage"
          <*> v .: "methods"
          <*> (parseWorkflowStateJSON =<< v .: "state")
          <*> v .: "owner"
          <*> v .: "address"
      invalid -> typeMismatch "Contract" invalid
    where
      parseScriptJSON script =
        case Parser.parseScript script of
          Left err     -> fail $ show err
          Right script -> pure script

      parseWorkflowStateJSON inp =
        case Parser.parseWorkflowState inp of
          Left err -> fail $ show err
          Right wfs -> pure wfs

instance B.Binary Contract where
  put = Utils.putBinaryViaSerialize
  get = Utils.getBinaryViaSerialize

instance Arbitrary Contract where
  arbitrary = genericArbitraryU

-- | Two Contracts are equal if their addresses are equal
instance Eq Contract where
  (==) c c' = address c == address c'

-- XXX: Implement full validation
validateContract :: Contract -> Bool
validateContract Contract {..} =
  and [ length methods > 0
      , isRight (Typecheck.signatures script)
      ]

-- | Digitally sign a contract with a private key.
signContract :: MonadRandom m => Key.PrivateKey -> Contract -> m Key.Signature
signContract = Key.signS

lookupVarGlobalStorage :: Text -> Contract -> Maybe Value
lookupVarGlobalStorage k c = Map.lookup (Storage.Key k) gs
  where
    gs = Storage.unGlobalStorage $ globalStorage c

-- | Returns the list of callable methods by any ledger account given the
-- current contract state. Restrictions are not taken into account.
callableMethods :: Contract -> [Method]
callableMethods c
  = filter (\m -> methodInputPlaces m `isSubWorkflow` state c)
    . scriptMethods
    . script
    $ c


-- | Allowed callers of a method
data PermittedCallers = Anyone | Restricted (Set (Address AAccount))
  deriving (Show, Generic)

instance Arbitrary PermittedCallers where
  arbitrary = genericArbitraryU

instance ToJSON PermittedCallers where
  toJSON = callersJSON

-- | Create a JSON value returning the sorted list of addresses by hash. This is
-- done instead of using the ToJSON instance for Address so that integration
-- tests can pass; They expect an ordered list of addresses.
callersJSON :: PermittedCallers -> A.Value
callersJSON callers =
  toJSON $
    case callers of
      Anyone -> []
      Restricted rs -> sortCallers rs
  where
    sortCallers = sort . toList

-- | Datatype used by Eval.hs to report callable methods after evaluating the
-- access restriction expressions associated with contract methods.
data CallableMethods = CallableMethods
  { cmCallableMethods    :: [CallableMethod]
  , cmNotCallableMethods :: [NotCallableMethod]
  } deriving (Show, Generic)

instance ToJSON CallableMethods where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance Arbitrary CallableMethods where
  arbitrary = genericArbitraryU

data CallableMethod = CallableMethod
  { cmName :: Name
  , cmArgs :: [Arg]
  } deriving (Show, Generic)

instance ToJSON CallableMethod where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance Arbitrary CallableMethod where
  arbitrary = genericArbitraryU

data NotCallableMethod = NotCallableMethod
  { ncmName :: Name
  , ncmArgs :: [Arg]
  , ncmReason :: NonEmpty NotCallableReason
  } deriving (Show, Generic)

instance ToJSON NotCallableMethod where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance Arbitrary NotCallableMethod where
  arbitrary = genericArbitraryU

-------------------------------------------------------------------------------


-- | Looks up a method with a given name in a Contract, taking into account the
-- current contract state. I.e. if a contract is in "terminal" state, no methods
-- will be returned.
lookupContractMethod
  :: Name
  -> Contract
  -> Maybe Method
lookupContractMethod nm = lookupMethod nm . script
