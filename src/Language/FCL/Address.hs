{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Language.FCL.Address where

import Protolude
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
  deriving (Eq, Ord, Show, Generic, Hash.Hashable, Binary, Serialize, FromJSON, ToJSON)

instance ToJSON ByteString where
  toJSON = toJSON . decodeUtf8

instance FromJSON ByteString where
  parseJSON v = do
    t :: Text <- parseJSON v
    pure $ encodeUtf8 t
    
instance Pretty (Address 'AAccount) where
  ppr (Address bs) = ppr bs

instance Pretty (Address 'AAsset) where
  ppr (Address bs) = ppr bs

instance Pretty (Address 'AContract) where
  ppr (Address bs) = ppr bs
