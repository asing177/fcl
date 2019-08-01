{-|

Storage for deployed contracts.

-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Language.FCL.Storage (
  -- ** Storage
  Key(..),
  Value(..),
  Storage,
  GlobalStorage(..),
  LocalStorage(..),

  -- ** Serialization
  decodeStorage,
  decodeLocalStorage,
  encodeStorage,
  storageSize,

  -- ** Validation
  validateStorage,
) where

import Protolude hiding (Type)

import Language.FCL.AST (Value(..))
import Language.FCL.Pretty (Pretty(..))

import Crypto.Number.Serialize (os2ip)

import qualified Language.FCL.Encoding as Encoding
import qualified Language.FCL.Hash as Hash

import Data.Serialize as S (Serialize, encode, decode, put, get)
import Data.Aeson as A hiding (Value(..), encode, decode)
import Data.Aeson.Types (typeMismatch, toJSONKeyText)
import qualified Data.Aeson as A
import qualified Data.Map as Map

import Test.QuickCheck

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

newtype Key = Key { unKey :: Text }
  deriving (Eq, Show, Generic, Ord, IsString)

instance Arbitrary Key where
  arbitrary = Key <$> arbitrary

type Storage = Map.Map Key Value

newtype GlobalStorage = GlobalStorage { unGlobalStorage :: Storage }
  deriving (Eq, Show, Generic, Hash.Hashable)

instance Arbitrary GlobalStorage where
  arbitrary = GlobalStorage <$> arbitrary

instance Semigroup GlobalStorage where
  (GlobalStorage m1) <> (GlobalStorage m2) = GlobalStorage (m1 <> m2)

instance Monoid GlobalStorage where
  mempty = GlobalStorage Map.empty

instance Pretty Key where
  ppr (Key key) = ppr key

newtype LocalStorage = LocalStorage { unLocalStorage :: Storage }
  deriving (Eq, Show, Generic, Hash.Hashable)

instance Semigroup LocalStorage where
  (LocalStorage m1) <> (LocalStorage m2) = LocalStorage (m1 <> m2)

instance Monoid LocalStorage where
  mempty = LocalStorage Map.empty

storageSize :: Storage -> Int
storageSize = Map.size

-- XXX
validateStorage :: Storage -> IO Bool
validateStorage storage = return True

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

decodeStorage :: ByteString -> Either [Char] Storage
decodeStorage = decode

encodeStorage :: Storage -> ByteString
encodeStorage = encode

decodeLocalStorage :: ByteString -> Either [Char] LocalStorage
decodeLocalStorage = decode

instance Serialize Key where
  put (Key bs) = S.put bs
  get = Key <$> S.get

instance Serialize GlobalStorage where
  put (GlobalStorage storage) = S.put storage
  get = GlobalStorage <$> S.get

instance Serialize LocalStorage where
  put (LocalStorage storage) = S.put storage
  get = LocalStorage <$> S.get

instance ToJSON GlobalStorage where
  toJSON = toJSON . unGlobalStorage

instance ToJSON LocalStorage where
  toJSON = toJSON . unLocalStorage

instance ToJSONKey Value where

instance FromJSON GlobalStorage where
  parseJSON = fmap GlobalStorage . parseJSON

instance FromJSON LocalStorage where
  parseJSON = fmap LocalStorage . parseJSON

instance ToJSON Value where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON Value where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance ToJSONKey Key where
  toJSONKey = toJSONKeyText unKey

instance FromJSONKey Key where
  fromJSONKey = A.FromJSONKeyText Key

instance ToJSON Key where
  toJSON  = toJSON . unKey

instance FromJSON Key where
  parseJSON v =
    case v of
      A.String s -> pure  $ Key s
      _ -> typeMismatch "Key" v

instance FromJSONKey Value where

-------------------------------------------------------------------------------
-- Hashing
-------------------------------------------------------------------------------

instance Hash.Hashable Key where
  toHash (Key bs) = Hash.toHash bs

base16HashToInteger :: Hash.Hash Encoding.Base16ByteString -> Integer
base16HashToInteger = os2ip
