{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Language.FCL.Address where

import Protolude
import GHC.Show
import Language.FCL.Hash as Hash (Hashable(..))
import Language.FCL.Pretty
import Data.Aeson (ToJSON(..), FromJSON(..), ToJSONKey(..), FromJSONKey(..))
import Data.Serialize as S (Serialize(..))
import Data.Binary (Binary(..))

-- | Type level tags of address type.
data AddrType
  = AAccount
  | AContract
  | AAsset
  deriving (Eq, Ord, Show, Generic, Serialize, Binary, FromJSON, ToJSON)

-- | An address is something that implements 'IsAddress'.
newtype Address (t :: AddrType)
  = Address ByteString
  deriving (Eq, Ord, Show, Generic, Hash.Hashable, Binary, Serialize)

-- class ToAddress o where
--   toAddress :: o -> Address a

instance FromJSON (Address a) where
  parseJSON = notImplemented

instance ToJSON (Address a) where
  toJSON = notImplemented

instance FromJSONKey (Address a) where
  fromJSONKey = notImplemented

instance ToJSONKey (Address a) where
  toJSONKey = notImplemented

-- instance Serialize (Address a) where
--   put = notImplemented
--   get = notImplemented

-- instance Binary (Address a) where
--   put = notImplemented
--   get = notImplemented


instance Pretty (Address a) where
  ppr = notImplemented

