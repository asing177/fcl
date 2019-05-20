{-|

Builtin operations for FCL evaluation. Operations that interact with
cryptographic primitives, network status, and ledger state.

-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.FCL.Prim (
  -- ** Data types
  PrimOp(..),
  AssetPrimOp(..),
  MapPrimOp(..),
  SetPrimOp(..),
  CollPrimOp(..),

  -- ** Mappings
  primName,
  lookupPrim
) where

import Protolude hiding (Hashable)
import Language.FCL.Hash (Hashable)
import Control.Arrow ((&&&))

import Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.List
import Data.Serialize (Serialize)

import Language.FCL.Pretty (Pretty(..))

data PrimOp
  = Verify              -- ^ @verify(addr,sig,msg)@                           : Verify a signature
  | Sign                -- ^ @sign(msg)@                                      : Sign a message
  | Block               -- ^ @block()@                                        : Active block
  | Deployer            -- ^ @deployer()@                                     : Deployer of contract
  | Sender              -- ^ @sender()@                                       : Transaction caller
  | Created             -- ^ @created()@                                      : Time of contract creation
  | Address             -- ^ @address()@                                      : Address of contract
  | Validator           -- ^ @validator()@                                    : Account address of validator
  | Sha256              -- ^ @sha256(any)@                                    : SHA256 digest of any data type turned into a string
  | AccountExists       -- ^ @accountExists(addr)@                            : Check if account exists in world state
  | AssetExists         -- ^ @assetExists(addr)@                              : Check if asset exists in world state
  | ContractExists      -- ^ @contractExists(addr)@                           : Check if contract exists in world state
  | Terminate           -- ^ @terminate()@                                    : Terminate contract execution
  | Now                 -- ^ @now()@                                          : Current date + time in UTC
  | TransitionTo        -- ^ @transitionTo(state)@                            : Transition to state msg
  | CurrentState        -- ^ @state()@                                        : Transition to state msg
  | TxHash              -- ^ @txHash()@                                       : Transaction hash
  | ContractValueExists -- ^ @contractValueExists(addr,varName)@              : Query a value's existence in a contract's global storage
  | ContractState       -- ^ @contractState(addr)@                            : Query the state of a smart contract
  | IsBusinessDayUK     -- ^ @isBusinessDayUK(datetime)@                      : Predicate checking if datetime is a business day or not
  | NextBusinessDayUK   -- ^ @nextBusinessDayUK(datetime)@                    : Returns the next business day after the supplied datetime
  | IsBusinessDayNYSE   -- ^ @isBusinessDayNYSE(datetime)@                    : Predicate checking if datetime is a business day or not
  | NextBusinessDayNYSE -- ^ @nextBusinessDayNYSE(datetime)@                  : Returns the next business day after the supplied datetime
  | Between             -- ^ @between(datetime,datetime,datetime)@            : Returns (True/False) if the first datetime is within the latter two
  | TimeDiff            -- ^ @timeDiff(datetime,datetime)@                    : Returns the differences in time between two datetimes
  | Round               -- ^ @round(n: int, num<m>): num<n>@                  : Round away from zero the second argument to precision given by first argument
  | RoundUp             -- ^ @roundUp(n: int, num<m>): num<n>@                : Round up the second argument to precision given by first argument
  | RoundDown           -- ^ @roundDown(n: int, num<m>): num<n>@              : Round down the second argument to precision given by first argument
  | RoundRem            -- ^ @roundRem(n: int, num<m>): num<m>@               : The remainder of rounding away from zero the second argument to precision given by first argument
  | RoundUpRem          -- ^ @roundUpRem(n: int, num<m>): num<m>@             : The remainder of rounding up the second argument to precision given by first argument
  | RoundDownRem        -- ^ @roundDownRem(n: int, num<m>): num<m>@           : The remainder of rounding down the second argument to precision given by first argument
  | ContractValue       -- ^ @contractValue(addr,varName)@                    : Query a value in the contract's global storage
  | Stay                -- ^ @stay()@                                         : Self-transition
  | AssetPrimOp AssetPrimOp
  | MapPrimOp MapPrimOp
  | SetPrimOp SetPrimOp
  | CollPrimOp CollPrimOp
  deriving (Eq, Show, Generic, Ord, Serialize, FromJSON, ToJSON, Hashable)

-- | These prim ops are "polymorphic" in the sense that their argument or return
-- types vary based on the type of asset that is passed as an argument
data AssetPrimOp
  = HolderBalance       -- ^ @holderBalance(asset,account)@
  | TransferTo          -- ^ @transferTo(asset,amount)@                       : Transfer n asset holdings to contract
  | TransferFrom        -- ^ @transferFrom(asset,amount,acc)@                 : Transfer n asset holdings from contract to account
  | CirculateSupply     -- ^ @circulate(asset,amount)@                        : Circulate n asset supply to issuer's holdings
  | TransferHoldings    -- ^ @transferHoldings(from,asset,amount,to)@         : Transfer asset holdings from account to account
  deriving (Eq, Show, Generic, Ord, Serialize, FromJSON, ToJSON, Hashable)

-- | These primops are polymorphic over the key and value type parameters of the
-- map supplied as an argument.
data MapPrimOp
  = MapInsert  -- ^ @mapInsert(key, value, map)@     : Insert an element into a map
  | MapDelete  -- ^ @mapDelete(key, map)@            : Delete an element from map. If the element doesn't exist, an error occurs.
  | MapLookup  -- ^ @lookup(key, map)@            : Lookup an element from a map
  | MapModify  -- ^ @modify(key, f, map)@         : Modify an element in the map
  deriving (Eq, Show, Generic, Ord, Enum, Bounded, Serialize, FromJSON, ToJSON, Hashable)

data SetPrimOp
  = SetInsert -- ^ @setInsert(value, set)@ : Insert an element into a set. If the element already exists, the original set is returned.
  | SetDelete -- ^ @setDelete(value, set)@ : Delete an element from a set. If the element does not exist, and error occurs.
  deriving (Eq, Show, Generic, Ord, Enum, Bounded, Serialize, FromJSON, ToJSON, Hashable)

data CollPrimOp
  = Aggregate  -- ^ @aggregate(v, f, coll)@   : Fold over a collection, accumulating a resulting value with 'f' using 'v' as the initial value
  | Transform  -- ^ @transform(f, coll)@      : Map over a collection, producing a new collection by applying 'f' to all values in the original
  | Filter     -- ^ @filter(p, coll)@         : Produce a new collection by removing the values that do not satisfy the supplied predicate
  | Element    -- ^ @element(v, coll)@        : Checks for membership of a value to a collection
  | IsEmpty    -- ^ @isEmpty(coll)@           : Checks if the given collection is empty or not
  deriving (Eq, Show, Generic, Ord, Enum, Bounded, Serialize, FromJSON, ToJSON, Hashable)

{-# INLINE primName #-}
primName :: IsString s => PrimOp -> s
primName = \case
  Verify              -> "verify"
  Sign                -> "sign"
  Block               -> "block"
  Deployer            -> "deployer"
  Sender              -> "sender"
  Created             -> "created"
  Address             -> "address"
  Validator           -> "validator"
  Sha256              -> "sha256"
  AccountExists       -> "accountExists"
  AssetExists         -> "assetExists"
  ContractExists      -> "contractExists"
  Terminate           -> "terminate"
  Now                 -> "now"
  TransitionTo        -> "transitionTo"
  CurrentState        -> "state"
  TxHash              -> "txHash"
  ContractValue       -> "contractValue"
  ContractValueExists -> "contractValueExists"
  ContractState       -> "contractState"
  IsBusinessDayUK     -> "isBusinessDayUK"
  NextBusinessDayUK   -> "nextBusinessDayUK"
  IsBusinessDayNYSE   -> "isBusinessDayNYSE"
  NextBusinessDayNYSE -> "nextBusinessDayNYSE"
  Between             -> "isBetween"
  TimeDiff            -> "timeDiff"
  Round               -> "round"
  RoundUp             -> "roundUp"
  RoundDown           -> "roundDown"
  RoundRem            -> "roundRem"
  RoundUpRem          -> "roundUpRem"
  RoundDownRem        -> "roundDownRem"
  Stay                -> "stay"
  AssetPrimOp a       -> assetPrimName a
  MapPrimOp m         -> mapPrimName m
  SetPrimOp m         -> setPrimName m
  CollPrimOp c        -> collPrimName c

assetPrimName :: IsString s => AssetPrimOp -> s
assetPrimName = \case
  TransferTo          -> "transferTo"
  TransferFrom        -> "transferFrom"
  CirculateSupply     -> "circulate"
  TransferHoldings    -> "transferHoldings"
  HolderBalance       -> "holderBalance"

mapPrimName :: IsString s => MapPrimOp -> s
mapPrimName = \case
  MapInsert -> "mapInsert"
  MapDelete -> "mapDelete"
  MapLookup -> "lookup"
  MapModify -> "modify"

setPrimName :: IsString s => SetPrimOp -> s
setPrimName = \case
  SetInsert -> "setInsert"
  SetDelete -> "setDelete"

collPrimName :: IsString s => CollPrimOp -> s
collPrimName = \case
  Aggregate -> "aggregate"
  Transform -> "transform"
  Filter    -> "filter"
  Element   -> "element"
  IsEmpty   -> "isEmpty"

prims :: IsString s => [(s, PrimOp)]
prims =
  (map (primName &&& identity)
  [ Verify
  , Sign
  , Block
  , Deployer
  , Sender
  , Created
  , Address
  , Validator
  , Sha256
  , AccountExists
  , AssetExists
  , ContractExists
  , Terminate
  , Now
  , TransitionTo
  , CurrentState
  , TxHash
  , ContractValue
  , ContractValueExists
  , ContractState
  , IsBusinessDayUK
  , NextBusinessDayUK
  , IsBusinessDayNYSE
  , NextBusinessDayNYSE
  , Between
  , TimeDiff
  , Round
  , RoundUp
  , RoundDown
  , RoundRem
  , RoundUpRem
  , RoundDownRem
  , Stay
  ]) ++ map (second AssetPrimOp) assetPrims
     ++ map (second MapPrimOp) mapPrims
     ++ map (second SetPrimOp) setPrims
     ++ map (second CollPrimOp) collPrims

assetPrims :: IsString s => [(s, AssetPrimOp)]
assetPrims =
  map (assetPrimName &&& identity)
    [ TransferTo
    , TransferFrom
    , CirculateSupply
    , TransferHoldings
    , HolderBalance
    ]

mapPrims :: IsString s => [(s, MapPrimOp)]
mapPrims =
  map (mapPrimName &&& identity)
    [minBound.. maxBound]

setPrims :: IsString s => [(s, SetPrimOp)]
setPrims =
  map (setPrimName &&& identity)
    [minBound.. maxBound]

collPrims :: IsString s => [(s, CollPrimOp)]
collPrims =
  map (collPrimName &&& identity)
    [minBound.. maxBound]

lookupPrim :: (Eq s, IsString s) => s -> Maybe PrimOp
lookupPrim p = Data.List.lookup p prims

instance Pretty PrimOp where
  ppr = ppr . (primName :: PrimOp -> Text)
