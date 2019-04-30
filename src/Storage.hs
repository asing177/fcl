{-|

Storage for deployed contracts.

--}

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TupleSections              #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Storage (
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
  hashStorage,

) where

import           Protolude                            hiding (Type)

import           Script                               (DateTime (..),
                                                       Value (..))
import qualified Script.Parser                        as Parser
import           Script.Pretty                        (Pretty (..), prettyPrint)

import           Control.Monad                        (fail)
import           Crypto.Number.Serialize              (os2ip)

import qualified Encoding
import qualified Hash

import           Datetime.Types

import           Data.Aeson                           (FromJSON (..),
                                                       FromJSONKey (..),
                                                       ToJSON (..),
                                                       ToJSONKey (..), object,
                                                       (.:), (.=))
import qualified Data.Aeson                           as A
import           Data.Aeson.Types                     (toJSONKeyText,
                                                       typeMismatch)
import qualified Data.Map                             as Map
import           Data.Scientific
import           Data.Serialize                       as S (Serialize, decode,
                                                            encode, get, put)

import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

newtype Key = Key { unKey :: Text }
  deriving (Eq, Show, Generic, Ord, NFData, IsString)

type Storage as ac c = Map.Map Key (Value as ac c)

newtype GlobalStorage as ac c = GlobalStorage { unGlobalStorage :: Storage as ac c }
  deriving (Eq, Show, Generic, NFData, Hash.Hashable)

instance Semigroup (GlobalStorage as ac c) where
  (GlobalStorage m1) <> (GlobalStorage m2) = GlobalStorage (m1 <> m2)

instance Monoid (GlobalStorage as ac c) where
  mempty = GlobalStorage Map.empty

instance Pretty Key where
  ppr (Key key) = ppr key

newtype LocalStorage as ac c= LocalStorage { unLocalStorage :: Storage as ac c }
  deriving (Eq, Show, Generic, NFData, Hash.Hashable)

instance Semigroup (LocalStorage as ac c) where
  (LocalStorage m1) <> (LocalStorage m2) = LocalStorage (m1 <> m2)

instance Monoid (LocalStorage as ac c) where
  mempty = LocalStorage Map.empty

storageSize :: Storage as ac c -> Int
storageSize = Map.size

-- XXX
validateStorage :: Storage as ac c-> IO Bool
validateStorage storage = return True

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

decodeStorage
  :: (Ord as, Ord ac, Ord c, Serialize as, Serialize ac, Serialize c)
  => ByteString -> Either [Char] (Storage as ac c)
decodeStorage = decode

encodeStorage
  :: (Ord as, Ord ac, Ord c, Serialize as, Serialize ac, Serialize c)
  => Storage as ac c -> ByteString
encodeStorage = encode

decodeLocalStorage
  :: (Ord as, Ord ac, Ord c, Serialize as, Serialize ac, Serialize c)
  => ByteString -> Either [Char] (LocalStorage as ac c)
decodeLocalStorage = decode

instance Serialize Key where
  put (Key bs) = S.put bs
  get = Key <$> S.get

instance (Ord as, Ord ac, Ord c, Serialize as, Serialize ac, Serialize c)
  => Serialize (GlobalStorage as ac c) where
  put (GlobalStorage storage) = S.put storage
  get = GlobalStorage <$> S.get

instance (Ord as, Ord ac, Ord c, Serialize as, Serialize ac, Serialize c)
  => Serialize (LocalStorage as ac c) where
  put (LocalStorage storage) = S.put storage
  get = LocalStorage <$> S.get

instance (ToJSON as, ToJSON ac, ToJSON c) => ToJSON (GlobalStorage as ac c) where
  toJSON = toJSON . unGlobalStorage

instance (ToJSON as, ToJSON ac, ToJSON c) => ToJSON (LocalStorage as ac c) where
  toJSON = toJSON . unLocalStorage

instance (ToJSON as, ToJSON ac, ToJSON c) => ToJSONKey (Value as ac c) where

instance (ToJSON as, ToJSON ac, ToJSON c) => ToJSON (Value as ac c) where
  toJSON = \case
     VInt n       -> object ["tag" .= ("VInt" :: Text), "contents" .= toJSON n]
     VFloat n     -> object ["tag" .= ("VFloat" :: Text), "contents" .= toJSON n]
     VFixed f     -> object ["tag" .= ("VFixed" :: Text), "contents" .= A.toJSON f]
     VBool n      -> object ["tag" .= ("VBool" :: Text), "contents" .= toJSON n]
     VVoid        -> object ["tag" .= ("VVoid" :: Text), "contents" .= A.Null]
     VSig sig     -> object ["tag" .= ("VSig" :: Text), "contents" .= A.toJSON sig]
     VText n       -> object ["tag" .= ("VText" :: Text), "contents" .= A.toJSON n]
     VAccount n   -> object ["tag" .= ("VAccount" :: Text), "contents" .= toJSON n]
     VAsset n     -> object ["tag" .= ("VAsset" :: Text), "contents" .= toJSON n]
     VContract n  -> object ["tag" .= ("VContract" :: Text), "contents" .= toJSON n]
     VDateTime n  -> object ["tag" .= ("VDateTime" :: Text), "contents" .= toJSON n]
     VTimeDelta n -> object ["tag" .= ("VTimeDelta" :: Text), "contents" .= toJSON n]
     VState n     -> object ["tag" .= ("VState" :: Text), "contents" .= toJSON (prettyPrint n)]
     VEnum c      -> object ["tag" .= ("VEnum" :: Text), "contents" .= toJSON c]
     VMap vmap    -> object ["tag" .= ("VMap" :: Text), "contents" .= toJSON vmap]
     VSet vset    -> object ["tag" .= ("VSet" :: Text), "contents" .= toJSON vset]
     VUndefined   -> object ["tag" .= ("VUndefined" :: Text), "contents" .= A.Null]

instance (Ord as, Ord ac, Ord c, FromJSON as, FromJSON ac, FromJSON c) => FromJSON (GlobalStorage as ac c) where
  parseJSON = fmap GlobalStorage . parseJSON

instance (Ord as, Ord ac, Ord c, FromJSON as, FromJSON ac, FromJSON c) => FromJSON (LocalStorage as ac c) where
  parseJSON = fmap LocalStorage . parseJSON

instance (Ord as, Ord ac, Ord c, FromJSON as, FromJSON ac, FromJSON c) => FromJSON (Value as ac c) where
  parseJSON v = case v of
    A.Array _  -> typeMismatch "Cannot parse array." v
    A.String _ -> typeMismatch "Please pass tagged objects, not json values" v
    A.Null     -> typeMismatch "Please pass tagged objects, not json values" v
    A.Bool _   -> typeMismatch "Please pass tagged objects, not json values" v
    A.Number _ -> typeMismatch "Please pass tagged objects, not json values" v
    A.Object o -> do
      constr :: Text <- o .: "tag"
      case constr of
        "VInt"      -> do
          c <- toBoundedInteger <$> (o .: "contents")
          case c of
            Just n  -> pure (VInt n)
            Nothing -> typeMismatch "Cannot parse unbounded integer." v
        "VFloat"      -> do
          c <- toBoundedRealFloat <$> (o .: "contents")
          case c of
            Right n -> pure (VFloat n)
            Left _  -> typeMismatch "Cannot parse unbounded float." v
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
        "VFixed"    -> VFixed    <$> o .: "contents"
        "VText"      -> VText      <$> o .: "contents"
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
          Left err  -> fail $ show err
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
      _          -> typeMismatch "Key" v

instance (Ord as, Ord ac, Ord c, FromJSON as, FromJSON ac, FromJSON c) => FromJSONKey (Value as ac c) where

-------------------------------------------------------------------------------

instance ToField Key where
  toField = toField . unKey

instance FromField Key where
  fromField f mdata = do
    bs <- fromField f mdata
    case bs of
      Nothing          -> returnError UnexpectedNull f ""
      Just (Left err)  -> returnError ConversionFailed f err
      Just (Right key) -> pure $ Key key

-------------------------------------------------------------------------------
-- Hashing
-------------------------------------------------------------------------------

instance Hash.Hashable Key where
  toHash (Key bs) = Hash.toHash bs

base16HashToInteger :: Hash.Hash Encoding.Base16ByteString -> Integer
base16HashToInteger = os2ip

hashStorage :: (Hash.Hashable as, Hash.Hashable ac, Hash.Hashable c) => Storage as ac c -> Integer
hashStorage = base16HashToInteger . Hash.toHash
