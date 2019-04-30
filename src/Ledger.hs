{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Ledger where

import           Asset
import           Contract
import           Data.Serialize
import           Protolude
import           Script
-- class World w where
--   accountExists :: ac -> w -> Bool
--   assetExists :: as -> w -> Bool
--   contractExists :: c -> w -> Bool
--   transferAsset :: as -> ac -> ac -> balance -> w -> Either err w
--   circulateAsset :: as -> ac -> balance -> w -> Either err w
--   lookupAccount :: ac -> w -> account
--   lookupAsset :: as -> w -> asset
--   lookupContract :: c -> w -> contract
--   calcBalance :: asset -> ac -> w -> Value as ac c

data AssetError as ac
  = InsufficientHoldings ac Integer
  | InsufficientSupply as Integer     -- [Char] for serialize instance
  | CirculatorIsNotIssuer ac as
  | SelfTransfer ac
  | HolderDoesNotExist ac
  deriving (Show, Eq, Generic, Serialize)

data AccountError ac
  = AccountDoesNotExist ac
  | AccountExists ac
  deriving (Show, Eq, Generic, Serialize)

data ContractError c
  = ContractDoesNotExist c
  | ContractExists c
  deriving (Show, Eq, Generic, Serialize)

data World as ac c asset account = World
  { transferAsset
    :: as
    -> Holder ac c
    -> Holder ac c
    -> Int64
    -> Either (AssetError ac as) (World as ac c asset account)

  , circulateAsset
    :: as
    -> ac
    -> Int64
    -> Either (AssetError ac as) (World as ac c asset account)

  , lookupAccount  :: ac -> Either (AccountError ac) account
  , lookupAsset    :: as -> Either (AssetError as ac) asset
  , lookupContract :: c -> Either (ContractError c) (Contract as ac c)
  , calcBalance    :: asset -> ac -> Value as ac c
  } deriving (Generic, NFData)

class Addressable o where
  toAddress :: o -> a
