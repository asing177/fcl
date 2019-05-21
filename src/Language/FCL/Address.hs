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
import GHC.Show

-- | Address conversion error, indexed by a type, to allow custom @Pretty@
-- instances.
data AddrConvErr a = ParseError

-- | A class that allows us to be parametric over different notions of Address.
class IsAddress a where
  byteStringToAddress :: ByteString -> Either (AddrConvErr a) a
  addressToByteString :: a -> ByteString

-- | Type level tags of address type.
data AddrType
  = AAccount
  | AContract
  | AAsset

-- | An address is something that implements 'IsAddress'.
data Address (a :: AddrType) = forall b. IsAddress b => MkAddress b

instance Eq (Address a) where
  MkAddress a1 == MkAddress a2 = addressToByteString a1 == addressToByteString a2

instance Ord (Address a) where
  compare (MkAddress a1) (MkAddress a2)
    = compare (addressToByteString a1) (addressToByteString a2)

instance Show (Address AAccount) where
  show (MkAddress a) = "u'" <> toS (addressToByteString a) <> "'"
instance Show (Address AAsset) where
  show (MkAddress a) = "a'" <> toS (addressToByteString a) <> "'"
instance Show (Address AContract) where
  show (MkAddress a) = "c'" <> toS (addressToByteString a) <> "'"


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

instance Hash.Hashable (Address AAccount) where
  toHash = notImplemented

instance Hash.Hashable (Address AAsset) where
  toHash = notImplemented

instance Hash.Hashable (Address AContract) where
  toHash = notImplemented

-- instance Serialize (Address a) where
--   put = notImplemented
--   get = notImplemented

-- instance Binary (Address a) where
--   put = notImplemented
--   get = notImplemented


instance Pretty (Address 'AAccount) where
  ppr (MkAddress a) = ppr . addressToByteString $ a

instance Pretty (Address 'AAsset) where
  ppr (MkAddress a) = ppr . addressToByteString $ a

instance Pretty (Address 'AContract) where
  ppr (MkAddress a) = ppr . addressToByteString $ a
