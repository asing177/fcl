{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
module Language.FCL.World where

import Protolude
import Data.Serialize
import Language.FCL.Asset
import Language.FCL.Address
import Language.FCL.Contract
-- import Language.FCL.Account
import qualified Language.FCL.Key as Key

data AssetError
  = InsufficientHoldings (Address AAsset) Balance
  | InsufficientSupply (Address AAsset) Balance
  | CirculatorIsNotIssuer Holder (Address AAsset)
  | AssetError
  | SelfTransfer Holder
  | HolderDoesNotExist Holder
  | AssetDoesNotExist (Address AAsset)
  | SenderDoesNotExist Holder
  | ReceiverDoesNotExist Holder
  deriving (Show, Eq, Generic, Serialize)

data AccountError
  = AccountDoesNotExist (Address AAccount)
  deriving (Show, Eq, Generic, Serialize)

data ContractError
  = ContractDoesNotExist (Address AContract)
  deriving (Show, Eq, Generic, Serialize)

class World w where
  type Account' w
  type Asset' w

  transferAsset
    :: Address AAsset
    -> Holder
    -> Holder
    -> Balance
    -> w
    -> Either AssetError w

  circulateAsset
    :: Address AAsset
    -> Address AAccount
    -> Balance
    -> w
    -> Either AssetError w

  lookupAccount :: Address AAccount -> w -> Either AccountError (Account' w)
  lookupAsset :: Address AAsset -> w -> Either AssetError (Asset' w)
  lookupContract :: Address AContract -> w -> Either ContractError Contract

  assetType :: (Asset' w) -> w -> AssetType
  assetBalance :: (Asset' w) -> Holder -> w -> Maybe Balance
  assetToAddr :: (Asset' w) -> w -> Address AAsset

  publicKey :: (Account' w) -> w -> Key.PubKey
  accountToAddr :: (Account' w) -> w -> Address AAccount



