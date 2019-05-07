{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Asset where

import           Hash
import           Protolude
import           Data.Serialize

data Holder ac c
  = AccountHolder ac
  | ContractHolder c
  deriving (Eq, Ord, Show, Generic, Hash.Hashable, Serialize)
