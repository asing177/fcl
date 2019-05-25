{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
module Language.FCL.Asset where

import Protolude
import Data.Serialize
import qualified Data.Binary as B
import Language.FCL.Address
import Language.FCL.Pretty (Pretty(..))
import Numeric.Lossless.Number (Decimal(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Language.FCL.Hash as Hash (Hashable(..))

data Holder
  = AccountHolder (Address AAccount)
  | ContractHolder (Address AContract)
  deriving (Eq, Ord, Show, Generic, Serialize)

holderToAccount :: Holder -> Address AAccount
holderToAccount (AccountHolder addr) = addr
holderToAccount _ = panic "Invalid holder to account coercion"

holderToContract :: Holder -> Address AContract
holderToContract (ContractHolder addr) = addr
holderToContract _ = panic "Invalid holder to contract coercion"

newtype Balance
  = Balance { unBalance :: Decimal }
  deriving (Eq, Ord, Show, Generic, B.Binary, Serialize, Hash.Hashable)

instance ToJSON Balance where
  toJSON (Balance x) = toJSON x

instance FromJSON Balance where
  parseJSON x = Balance <$> parseJSON x

instance Num Balance where
  (+) (Balance a) (Balance b) = Balance (a + b)
  (*) (Balance a) (Balance b) = Balance (a * b)
  abs           = Balance . abs . unBalance
  signum        = Balance . signum . unBalance
  negate        = Balance . negate . unBalance
  fromInteger   = Balance . fromInteger

instance Pretty Balance where
  ppr (Balance n) = ppr n

-- | Type of an asset's value. Underlying value is always a Int64, but this
-- informs the representation and range of valid values.
data AssetType
  = Discrete               -- ^ Discrete (Non-zero integer value)
  | Fractional Integer     -- ^ Fractional (Fixed point decimal value)
  | Binary                 -- ^ Binary (Held/Not-Held) (supply is +1 for held, 0 for not-held)
  deriving (Eq, Ord, Show, Read, Generic, Hash.Hashable)
