{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Language.FCL.Address where

import Protolude
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
data Address (a :: AddrType) = forall a. IsAddress a => Address a

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
