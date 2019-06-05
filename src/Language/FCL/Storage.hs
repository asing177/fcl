{-|

Storage for deployed contracts.

--}

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

  -- ** Hashing
  -- hashStorage,

) where

import Protolude hiding (Type)

import Language.FCL.AST (Value(..), DateTime(..))
import Language.FCL.Pretty (Pretty(..), prettyPrint)
import qualified Language.FCL.Parser as Parser

import Control.Monad (fail)
import Crypto.Number.Serialize (os2ip)

import qualified Language.FCL.Encoding as Encoding
import qualified Language.FCL.Hash as Hash

import Datetime.Types

import Data.Serialize as S (Serialize, encode, decode, put, get)
import Data.Aeson (FromJSONKey(..), ToJSONKey(..), ToJSON(..), FromJSON(..), object, (.=), (.:))
import Data.Aeson.Types (typeMismatch, toJSONKeyText)
import qualified Data.Aeson as A
import qualified Data.Map as Map

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

newtype Key = Key { unKey :: Text }
  deriving (Eq, Show, Generic, Ord, IsString)

type Storage = Map.Map Key Value

newtype GlobalStorage = GlobalStorage { unGlobalStorage :: Storage }
  deriving (Eq, Show, Generic, Hash.Hashable)

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

instance ToJSON Value where
  toJSON = \case
    VNum n       -> object ["tag" .= ("VNum" :: Text), "contents" .= toJSON n]
    VBool n      -> object ["tag" .= ("VBool" :: Text), "contents" .= toJSON n]
    VVoid        -> object ["tag" .= ("VVoid" :: Text), "contents" .= A.Null]
    VSig sig     -> object ["tag" .= ("VSig" :: Text), "contents" .= A.toJSON sig]
    VText n      -> object ["tag" .= ("VText" :: Text), "contents" .= A.toJSON n]
    VAccount n   -> object ["tag" .= ("VAccount" :: Text), "contents" .= toJSON n]
    VAsset n     -> object ["tag" .= ("VAsset" :: Text), "contents" .= toJSON n]
    VContract n  -> object ["tag" .= ("VContract" :: Text), "contents" .= toJSON n]
    VDateTime n  -> object ["tag" .= ("VDateTime" :: Text), "contents" .= toJSON n]
    VTimeDelta n -> object ["tag" .= ("VTimeDelta" :: Text), "contents" .= toJSON n]
    VState n     -> object ["tag" .= ("VState" :: Text), "contents" .= toJSON (prettyPrint n)]
    -- VConstr c vs -> object ["tag" .= ("VConstr" :: Text), "contents" .= toJSON c]
    VMap vmap    -> object ["tag" .= ("VMap" :: Text), "contents" .= toJSON vmap]
    VSet vset    -> object ["tag" .= ("VSet" :: Text), "contents" .= toJSON vset]
    VUndefined   -> object ["tag" .= ("VUndefined" :: Text), "contents" .= A.Null]

instance FromJSON GlobalStorage where
  parseJSON = fmap GlobalStorage . parseJSON

instance FromJSON LocalStorage where
  parseJSON = fmap LocalStorage . parseJSON

instance FromJSON Value where
  parseJSON v = case v of
    A.Array _  -> typeMismatch "Cannot parse array." v
    A.String _ -> typeMismatch "Please pass tagged objects, not json values" v
    A.Null     -> typeMismatch "Please pass tagged objects, not json values" v
    A.Bool _   -> typeMismatch "Please pass tagged objects, not json values" v
    A.Number _ -> typeMismatch "Please pass tagged objects, not json values" v
    A.Object o -> do
      constr :: Text <- o .: "tag"
      case constr of
        "VNum"      -> VNum <$> (o .: "contents")
        "VBool"     -> VBool     <$> o .: "contents"
        "VAccount"  -> VAccount  <$> o .: "contents"
        "VAsset"    -> VAsset    <$> o .: "contents"
        "VContract" -> VContract <$> o .: "contents"
        "VDateTime"-> do
            c <- parseDatetime <$> (o .: "contents")
            case c of
              (Just dt) -> pure $ VDateTime $ DateTime dt
              Nothing -> typeMismatch "Invalid date format, expecting ISO8601, given:" v
        "VTimeDelta" -> VTimeDelta  <$> o .: "contents"
        "VSig"      -> VSig      <$> o .: "contents"
        "VText"     -> VText     <$> o .: "contents"
        "VEnum"     -> VEnum     <$> o .: "contents"
        "VMap"      -> VMap      <$> o .: "contents"
        "VSet"      -> VSet      <$> o .: "contents"
        "VState"    -> VState <$> (parseWorkflowStateJSON =<< o .: "contents")
        "VVoid"     -> pure VVoid
        "VUndefined" -> pure VUndefined
        tag -> typeMismatch "Value tag as a string" v
    where
      parseWorkflowStateJSON inp =
        case Parser.parseWorkflowState inp of
          Left err -> fail $ show err
          Right wfs -> pure wfs

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

-- hashStorage :: Storage -> Integer
-- hashStorage = base16HashToInteger . Hash.toHash
