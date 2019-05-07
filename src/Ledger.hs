{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ledger where

import           Asset
import           Contract
import qualified Data.Map       as Map
import           Data.Serialize
import           Protolude
import           Script

-- TODO: Change Int64 to some generic balance
data AssetError as ac c
  = InsufficientHoldings ac Int64
  | InsufficientSupply as Int64     -- [Char] for serialize instance
  | CirculatorIsNotIssuer (Holder ac c) as
  | SelfTransfer (Holder ac c)
  | HolderDoesNotExist (Holder ac c)
  | AssetDoesNotExist as
  deriving (Show, Eq, Generic, Serialize)

data AccountError ac
  = AccountDoesNotExist ac
  | AccountExists ac
  deriving (Show, Eq, Generic, Serialize)

data ContractError c
  = ContractDoesNotExist c
  | ContractExists c
  deriving (Show, Eq, Generic, Serialize)

class WorldOps w where
  transferAsset
    :: (Num balance)
    => w
    -> as
    -> Holder ac c
    -> Holder ac c
    -> balance
    -> Either (AssetError as ac c) w

  circulateAsset
    :: (Num balance)
    => w
    -> as
    -> ac
    -> balance
    -> Either (AssetError as ac c) w

  lookupAccount :: w -> ac -> Either (AccountError ac) account
  lookupAsset :: w -> as -> Either (AssetError as ac c) asset
  lookupContract :: w -> c -> Either (ContractError c) (Contract as ac c)
  calcBalance :: w -> asset -> ac -> Value as ac c

class Addressable o a where
  toAddress :: o -> a
