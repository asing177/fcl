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

import Data.Serialize as S (Serialize, encode, decode)
import Data.Aeson as A hiding (Value(..), encode, decode)
import Data.Aeson.Types (typeMismatch, toJSONKeyText)
import qualified Data.Aeson as A
import qualified Data.Map as Map

import Test.QuickCheck

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

newtype Key = Key { unKey :: Text }
  deriving (Eq, Show, Generic, Ord, IsString, ToJSON, Pretty, Serialize)

instance Arbitrary Key where
  arbitrary = Key <$> arbitrary

type Storage = Map.Map Key Value

newtype GlobalStorage = GlobalStorage { unGlobalStorage :: Storage }
  deriving (Eq, Show, Generic, Hash.Hashable, Serialize, ToJSON, FromJSON)

instance Arbitrary GlobalStorage where
  arbitrary = GlobalStorage <$> arbitrary

instance Semigroup GlobalStorage where
  (GlobalStorage m1) <> (GlobalStorage m2) = GlobalStorage (m1 <> m2)

instance Monoid GlobalStorage where
  mempty = GlobalStorage Map.empty

newtype LocalStorage = LocalStorage { unLocalStorage :: Storage }
  deriving (Eq, Show, Generic, Hash.Hashable, Serialize, ToJSON, FromJSON)

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

instance ToJSONKey Key where
  toJSONKey = toJSONKeyText unKey

instance FromJSONKey Key where
  fromJSONKey = A.FromJSONKeyText Key

instance FromJSON Key where
  parseJSON v =
    case v of
      A.String s -> pure  $ Key s
      _ -> typeMismatch "Key" v

-------------------------------------------------------------------------------
-- Hashing
-------------------------------------------------------------------------------

instance Hash.Hashable Key where
  toHash (Key bs) = Hash.toHash bs

base16HashToInteger :: Hash.Hash Encoding.Base16ByteString -> Integer
base16HashToInteger = os2ip
