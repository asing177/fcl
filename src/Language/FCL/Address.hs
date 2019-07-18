{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Language.FCL.Address (
  AddrType(..),
  Address(..),
) where

import Protolude
import Test.QuickCheck
import Language.FCL.Hash as Hash (Hashable(..), Hash(..), getRawHash)
import Language.FCL.Encoding as Encoding
import Language.FCL.Pretty
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Serialize as S (Serialize(..))
import Data.Binary (Binary(..))

import Language.FCL.Orphans ()

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

instance Pretty (Address 'AAccount) where
  ppr (Address bs) = ppr bs

instance Pretty (Address 'AAsset) where
  ppr (Address bs) = ppr bs

instance Pretty (Address 'AContract) where
  ppr (Address bs) = ppr bs

----------------
-- Arbitrary --
---------------

instance Arbitrary (Address a) where
  arbitrary = Address . Hash.getRawHash <$> (arbitrary ::  Gen (Hash.Hash Encoding.Base58ByteString))
