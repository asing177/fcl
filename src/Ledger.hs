{-|

Ledger world state.

-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module Ledger where

import Protolude hiding (from, to, get, put)

import Control.Arrow ((&&&))

import Data.Aeson (ToJSON, FromJSON, eitherDecode)
import qualified Data.Binary as B
import Data.Serialize (Serialize)
import qualified Data.Map.Strict as Map

import Asset (Holder(..))
import Contract (Contract)

import Asset
import qualified Contract
import qualified Utils
import Script

data AssetError
  = InsufficientHoldings Addr Balance
  | InsufficientSupply Addr Balance     -- [Char] for serialize instance
  | CirculatorIsNotIssuer Holder Addr
  | SelfTransfer Holder
  | HolderDoesNotExist Holder
  | AssetDoesNotExist Addr
  deriving (Show, Eq, Generic, Serialize)

data AccountError
  = AccountDoesNotExist Addr
  | AccountExists Addr
  deriving (Show, Eq, Generic, Serialize)

data ContractError
  = ContractDoesNotExist Addr
  | ContractExists Addr
  deriving (Show, Eq, Generic, Serialize)

class WorldOps w where
  transferAsset
    :: w
    -> Addr
    -> Holder
    -> Holder
    -> Balance
    -> Either AssetError w

  circulateAsset
    :: w
    -> Addr
    -> Addr
    -> Balance
    -> Either AssetError w

  lookupAccount :: w -> Addr -> Either AccountError account
  lookupAsset :: w -> Addr -> Either AssetError asset
  lookupContract :: w -> Addr -> Either ContractError Contract
  calcBalance :: w -> asset -> Addr -> Value

