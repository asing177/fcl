{-|

Export constructors of types that are not supposed to

-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.FCL.Unsafe where

import Protolude
import qualified Data.Binary as BI

-- | Strings safe for network serialization
newtype SafeString = SafeString ByteString
  deriving (Read, Eq, Ord, IsString, Generic, BI.Binary)

-- | Integers safe for serialization
newtype SafeInteger = SafeInteger Integer
  deriving (Eq, Ord, Read, NFData, Generic)
