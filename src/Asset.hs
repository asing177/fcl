{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Asset where

import           Hash
import           Protolude
import           Data.Serialize
import Script
import Numeric.Lossless.Number (Decimal(..))

-- | A quantity of units of value in an asset.
type Balance = Decimal

data Holder
  = AccountHolder Addr
  | ContractHolder Addr
  deriving (Eq, Ord, Show, Generic, Hash.Hashable, Serialize)
