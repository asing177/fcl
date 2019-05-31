{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Language.FCL.World (
  World(..),
  ContractError(..),
) where

import Protolude
import Data.Serialize
import Language.FCL.Asset
import Language.FCL.Address
import Language.FCL.Contract
import qualified Language.FCL.Key as Key

data ContractError
  = ContractDoesNotExist (Address AContract)
  deriving (Show, Eq, Generic, Serialize)

class World w where
  type Account' w
  type Asset' w
  type AssetError' w
  type AccountError'  w

  transferAsset
    :: Address AAsset
    -> Holder
    -> Holder
    -> Balance
    -> w
    -> Either (AssetError' w) w

  circulateAsset
    :: Address AAsset
    -> Address AAccount
    -> Balance
    -> w
    -> Either (AssetError' w) w

  lookupAccount :: Show ((AccountError' w)) => Address AAccount -> w -> Either (AccountError' w) (Account' w)
  lookupAsset :: Show ((AssetError' w)) => Address AAsset -> w -> Either (AssetError' w) (Asset' w)
  lookupContract :: Address AContract -> w -> Either ContractError Contract

  -- All the methods below have ambiguous types (hence -XAmbiguousTypes) which
  -- need to be resolved via type applications.

  assetType :: (Asset' w) -> AssetType
  assetBalance :: (Asset' w) -> Holder -> Maybe Balance
  assetToAddr :: (Asset' w) -> Address AAsset

  publicKey :: (Account' w) -> Key.PubKey
  accountToAddr :: (Account' w) -> Address AAccount
