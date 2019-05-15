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

-- | Address conversion error, indexed by a type, to allow custom @Pretty@
-- instances.
data AddrConvErr a = ParseError
  deriving (Show)

-- | A class that allows us to be parametric over different notions of Address.
class IsAddress a where
  byteStringToAddress :: ByteString -> Either (AddrConvErr a) a
  addressToByteString :: a -> ByteString

-- | Type level tags of address type.
data AddrType
  = AAccount
  | AContract
  | AAsset
  deriving (Eq, Ord, Show, Generic, Serialize, Binary, FromJSON, ToJSON)

-- | An address is something that implements 'IsAddress'.
data Address (t :: AddrType)
  = forall a. (IsAddress a) => Address a

instance FromJSON (Address a) where
  parseJSON = notImplemented

instance ToJSON (Address a) where
  toJSON = notImplemented

instance FromJSONKey (Address a) where
  fromJSONKey = notImplemented

instance ToJSONKey (Address a) where
  toJSONKey = notImplemented

instance Serialize (Address a) where
  put = notImplemented
  get = notImplemented

instance Binary (Address a) where
  put = notImplemented
  get = notImplemented

instance Eq (Address a) where
  Address a1 == Address a2 = addressToByteString a1 == addressToByteString a2

instance Ord (Address a) where
  compare (Address a1) (Address a2)
    = compare (addressToByteString a1) (addressToByteString a2)

instance Show (Address AAccount) where
  show (Address a) = "u'" <> toS (addressToByteString a) <> "'"
instance Show (Address AAsset) where
  show (Address a) = "a'" <> toS (addressToByteString a) <> "'"
instance Show (Address AContract) where
  show (Address a) = "c'" <> toS (addressToByteString a) <> "'"

instance Hash.Hashable (Address AAccount) where
  toHash = notImplemented
instance Hash.Hashable (Address AAsset) where
  toHash = notImplemented
instance Hash.Hashable (Address AContract) where
  toHash = notImplemented

instance Pretty (Address a) where
  ppr = notImplemented
