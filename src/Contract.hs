{-|

Contract datatypes, signing and operations.

-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}

module Contract (
  -- ** Types
  Contract(..),
  InvalidMethodName(..),

  callableMethods,
  PermittedCallers(..),
  CallableMethods,
--  callableMethodsJSON,
  callableMethods',

  -- ** Validation
  validateContract,

  -- ** Querying contract state
  lookupVarGlobalStorage,
  lookupContractMethod,

  -- ** Signing
  -- signContract,
) where

import Protolude hiding (state)

import Time (Timestamp)
import Storage (GlobalStorage)
import Control.Monad

-- import qualified Key
import qualified Hash
import qualified Storage
import qualified Utils

import Script (Method, Name, Script, WorkflowState, isSubWorkflow, lookupMethod, methodInputPlaces, scriptMethods)
import Script.Pretty ((<+>), ppr)
import qualified Script
import qualified Script.Pretty as Pretty
import qualified Script.Parser as Parser
import qualified Script.Typecheck as Typecheck

import Crypto.Random.Types (MonadRandom(..))
import Data.Serialize (Serialize)
import qualified Data.Map.Strict as Map
import qualified Data.Binary as B

import Data.Aeson (ToJSON(..), (.=), (.:))
import Data.Aeson.Types (typeMismatch)
import qualified Data.Aeson as A

-------------------------------------------------------------------------------
-- Contracts
-------------------------------------------------------------------------------

-- | A contract is a 4-tuple of the address of publisher, timestamp of
-- deployment time, script source, and it's initial storage hash.
data Contract as ac c = Contract
  { timestamp        :: Timestamp             -- ^ Timestamp of issuance
  , script           :: Script as ac c                -- ^ Underlying contract logic
  , globalStorage    :: GlobalStorage as ac c         -- ^ Initial state of the contract
  , methods          :: [Script.Name]         -- ^ Public methods
  , state            :: WorkflowState         -- ^ State of Contract
  , owner            :: ac      -- ^ Creator of the contract
  , address          :: c     -- ^ Contract Address, derived during creation
  } deriving (Eq, Show, Generic, NFData, Serialize, Hash.Hashable)

-- | Two Contracts are equal if their addresses are equal
-- instance (Eq as, Eq ac, ) => Eq Contract where
--   (==) c c' = address c == address c'

-- XXX: Implement full validation
validateContract :: (Ord as, Ord ac, Ord c) => Contract as ac c -> Bool
validateContract Contract {..} =
  and [ length methods > 0
      , isRight (Typecheck.signatures script)
      ]

-- | Digitally sign a contract with a private key.
-- signContract :: MonadRandom m => Key.PrivateKey -> Contract -> m Key.Signature
-- signContract = Key.signS

lookupVarGlobalStorage :: Text -> Contract as ac c -> Maybe (Script.Value as ac c)
lookupVarGlobalStorage k c = Map.lookup (Storage.Key k) gs
  where
    gs = Storage.unGlobalStorage $ globalStorage c

-- | Returns the list of callable methods by any ledger account given the
-- current contract state. Restrictions are not taken into account.
callableMethods :: Contract as ac c -> [Method as ac c]
callableMethods c =
  callableMethods' (Contract.state c) (Contract.script c)

callableMethods' :: WorkflowState -> Script as ac c -> [Method as ac c]
callableMethods' wfs s =
    filter (\m -> methodInputPlaces m `isSubWorkflow` wfs) methods
  where
    methods = scriptMethods s

-- | Allowed callers of a method
data PermittedCallers ac = Anyone | Restricted (Set ac)

-- -- | Create a JSON value returning the sorted list of addresses by hash. This is
-- -- done instead of using the ToJSON instance for Address such that integration
-- -- tests can pass; They expect an ordered list of addresses.
-- callersJSON :: PermittedCallers ac -> A.Value
-- callersJSON callers =
--   toJSON $
--     case callers of
--       Anyone -> []
--       Restricted rs -> sortCallers rs
--   where
--     addrHash = decodeUtf8 . Hash.getRawHash . unAddress
--     sortCallers = sort . map addrHash . toList

-- -- | Datatype used by Eval.hs to report callable methods after evaluating the
-- -- access restriction expressions associated with contract methods.
type CallableMethods ac = Map.Map Name (PermittedCallers ac, [(Name, Script.Type)])

-- callableMethodsJSON :: CallableMethods ac -> A.Value
-- callableMethodsJSON = toJSON
--                     . Map.mapKeys Pretty.prettyPrint
--                     . Map.map (bimap callersJSON (map (bimap Pretty.prettyPrint Pretty.prettyPrint)))

-------------------------------------------------------------------------------

data InvalidMethodName
  = MethodDoesNotExist Name
  | MethodNotCallable  Name WorkflowState
  deriving (Eq, Show, Generic, NFData, Serialize, Hash.Hashable)

-- | Looks up a method with a given name in a Contract, taking into account the
-- current contract state. I.e. if a contract is in "terminal" state, no methods
-- will be returned.
lookupContractMethod
  :: (Eq as, Eq ac, Eq c) => Script.Name
  -> Contract as ac c
  -> Either InvalidMethodName (Script.Method as ac c)
lookupContractMethod nm c =
  case lookupMethod nm (script c) of
    Nothing     -> Left $ MethodDoesNotExist nm
    Just method
      | method `elem` callableMethods c -> Right method
      | otherwise -> Left $ MethodNotCallable nm (state c)

instance Pretty.Pretty InvalidMethodName where
  ppr (MethodDoesNotExist nm) = "Method does not exist:" <+> ppr nm
  ppr (MethodNotCallable nm state) = "Method" <+> ppr nm <+> "not callable in current state:" <+> ppr state

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

-- instance
--   (Ord as, Ord ac, Ord c
--   ,Pretty.Pretty as, Pretty.Pretty ac, Pretty.Pretty c
--   ,A.ToJSON as, A.ToJSON ac, A.ToJSON c) =>  A.ToJSON (Contract as ac c) where
--   toJSON Contract{..} = A.object
--     [ "timestamp"     .= timestamp
--     , "script"        .= Pretty.prettyPrint script
--     , "storage"       .= globalStorage
--     , "methods"       .= methods
--     , "state"         .= state
--     , "owner"         .= owner
--     , "address"       .= address
--     ]

-- We can't define fromJSON without knowing how to parse Addresses
-- instance (A.FromJSON as, A.FromJSON ac, A.FromJSON c) =>  A.FromJSON (Contract as ac c) where
--   parseJSON = \case
--       A.Object v ->
--         Contract
--           <$> v .: "timestamp"
--           <*> (parseScriptJSON =<< v .: "script")
--           <*> v .: "storage"
--           <*> v .: "methods"
--           <*> (parseWorkflowStateJSON =<< v .: "state")
--           <*> v .: "owner"
--           <*> v .: "address"
--       invalid -> typeMismatch "Contract" invalid
--     where
--       parseScriptJSON script =
--         case Parser.parseScript script of
--           Left err     -> fail $ show err
--           Right script -> pure script

--       parseWorkflowStateJSON inp =
--         case Parser.parseWorkflowState inp of
--           Left err -> fail $ show err
--           Right wfs -> pure wfs

instance
  (Ord ac, Ord as, Ord c
  ,Serialize as, Serialize ac, Serialize c
  ,B.Binary as, B.Binary ac, B.Binary c) => B.Binary (Contract as ac c) where
  put = Utils.putBinaryViaSerialize
  get = Utils.getBinaryViaSerialize
