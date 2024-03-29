{-|

Byte encoding schemes.

-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Language.FCL.Encoding (
  -- ** ByteStringEncoding
  ByteStringEncoding(..),
  EncodingError(..),
  Base16ByteString(..),
  encodeBase16,
  decodeBase16E,
  Base58ByteString(..),
  encodeBase58,
  decodeBase58M,
  Base64ByteString(..),
  encodeBase64,
  decodeBase64E,
  Base64PByteString(..),
  encodeBase64P,
  decodeBase64PE
) where

import Protolude hiding (Show, show)
import Prelude (Show(..))
import Control.Monad (fail)
import Data.Aeson
import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()
import qualified Data.Binary as B
import qualified Data.ByteArray as B
import qualified Data.ByteArray.Encoding as B
import qualified Data.ByteString.Base58 as B58
import Data.Serialize as S

import Language.FCL.Pretty (Pretty(..))

data EncodingError a
  = BadEncoding a
  deriving (Show)

class (B.ByteArrayAccess a, S.Serialize a) => ByteStringEncoding a where
  encodeBase :: ByteString -> a                               -- ^ Encode a ByteString with a given byte encoding.
  decodeBase :: a -> ByteString                               -- ^ Decode ByteStringEncoding to ByteString.
  unbase :: a -> ByteString                                   -- ^ Unwrap ByteStringEncoding to ByteString without decoding.
  parseEncodedBS :: ByteString -> Either (EncodingError a) a  -- ^ Check whether a ByteString is valid according to a given byte encoding.
                                                              -- Wrap it with ByteStringEncoding if valid.

-------------------------------------------------------------------------------
-- Base58 Encoding
-------------------------------------------------------------------------------

-- | Base-58 encode a bytestring.
-- Uses the bitcoin alphabet.
--
-- > 123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz
newtype Base58ByteString = Base58ByteString
  { unbase58 :: ByteString
  } deriving (Show, B.ByteArrayAccess, S.Serialize, Eq, Ord, Semigroup, Monoid, Read, B.Binary, Pretty, Generic)

instance ByteStringEncoding Base58ByteString where
  encodeBase = Base58ByteString . B58.encodeBase58 B58.bitcoinAlphabet
  decodeBase b = fromMaybe (panic "Invalid Base58ByteString") (decodeBase58M b)
  unbase = unbase58
  parseEncodedBS bs = do
    let b58bs = Base58ByteString bs
    case decodeBase58M b58bs of
      Nothing -> Left $ BadEncoding b58bs
      Just _  -> Right b58bs


instance Arbitrary Base58ByteString where
  arbitrary = encodeBase . toS <$> (arbitrary @Text)

decodeBase58M :: Base58ByteString -> Maybe ByteString
decodeBase58M = B58.decodeBase58 B58.bitcoinAlphabet . unbase58

encodeBase58 :: ByteString -> Base58ByteString
encodeBase58 = encodeBase

instance FromJSON Base58ByteString where
  parseJSON v = do
    t :: Text <- parseJSON v
    let bs = encodeUtf8 t
    case parseEncodedBS bs of
      Left err -> fail (show err)
      Right baseBS -> pure baseBS

instance ToJSON Base58ByteString where
  toJSON = toJSON . decodeUtf8 . unbase

instance FromJSONKey Base58ByteString where
  fromJSONKey = (Base58ByteString . encodeUtf8) <$> fromJSONKey

-------------------------------------------------------------------------------
-- Base 16
-------------------------------------------------------------------------------

newtype Base16ByteString = Base16ByteString
  { unbase16 :: ByteString
  } deriving (Show, B.ByteArrayAccess, S.Serialize, Eq, Ord, Semigroup, Monoid, Read, B.Binary, Generic)

instance ByteStringEncoding Base16ByteString where
  encodeBase = Base16ByteString . B.convertToBase B.Base16
  decodeBase b = case decodeBase16E b of
   Left err -> panic "Invalid Base16ByteString"
   Right bs -> bs
  unbase = unbase16
  parseEncodedBS bs = do
    let b16bs = Base16ByteString bs
    case decodeBase16E b16bs of
      Left _ -> Left $ BadEncoding b16bs
      Right _  -> Right b16bs


instance Arbitrary Base16ByteString where
  arbitrary = encodeBase . toS <$> (arbitrary @Text)

decodeBase16E :: Base16ByteString -> Either [Char] ByteString
decodeBase16E b = B.convertFromBase B.Base16 (unbase16 b)

encodeBase16 :: ByteString -> Base16ByteString
encodeBase16 = encodeBase

instance FromJSON Base16ByteString where
  parseJSON v = do
    t :: Text <- parseJSON v
    let bs = encodeUtf8 t
    case parseEncodedBS bs of
      Left err -> fail (show err)
      Right baseBS -> pure baseBS

instance ToJSON Base16ByteString where
  toJSON = toJSON . decodeUtf8 . unbase
-------------------------------------------------------------------------------
-- Base 64
-------------------------------------------------------------------------------

newtype Base64ByteString = Base64ByteString
 { unbase64 :: ByteString
 } deriving (Show, B.ByteArrayAccess, S.Serialize, Eq, Ord, Semigroup, Monoid, Read, B.Binary, Generic)

instance ByteStringEncoding Base64ByteString where
  encodeBase = Base64ByteString . B.convertToBase B.Base64URLUnpadded
  decodeBase b = case decodeBase64E b of
    Left err -> panic "Invalid Base64ByteString"
    Right bs -> bs
  unbase = unbase64
  parseEncodedBS bs = do
    let b64bs = Base64ByteString bs
    case decodeBase64E b64bs of
      Left _ -> Left $ BadEncoding b64bs
      Right _  -> Right b64bs

instance Arbitrary Base64ByteString where
  arbitrary = encodeBase . toS <$> (arbitrary @Text)

decodeBase64E :: Base64ByteString -> Either [Char] ByteString
decodeBase64E b = B.convertFromBase B.Base64URLUnpadded (unbase64 b)

encodeBase64 :: ByteString -> Base64ByteString
encodeBase64 = encodeBase

instance FromJSON Base64ByteString where
  parseJSON v = do
    t :: Text <- parseJSON v
    let bs = encodeUtf8 t
    case parseEncodedBS bs of
      Left err -> fail (show err)
      Right baseBS -> pure baseBS

instance ToJSON Base64ByteString where
  toJSON = toJSON . decodeUtf8 . unbase

-------------------------------------------------------------------------------
-- Base 64 (Padded)
-------------------------------------------------------------------------------

-- | Base 64 hash (padded)
newtype Base64PByteString = Base64PByteString
  { unbase64P :: ByteString
  } deriving (Show, B.ByteArrayAccess, S.Serialize, Eq, Ord, Semigroup, Monoid, Read, B.Binary, Generic)

instance ByteStringEncoding Base64PByteString where
  encodeBase = Base64PByteString . B.convertToBase B.Base64
  decodeBase b = case decodeBase64PE b of
    Left err -> panic "Invalid Base64PByteString"
    Right bs -> bs
  unbase = unbase64P
  parseEncodedBS bs = do
    let b64Pbs = Base64PByteString bs
    case decodeBase64PE b64Pbs of
      Left _ -> Left $ BadEncoding b64Pbs
      Right _  -> Right b64Pbs

instance Arbitrary Base64PByteString where
  arbitrary = encodeBase . toS <$> (arbitrary @Text)

decodeBase64PE :: Base64PByteString -> Either [Char] ByteString
decodeBase64PE b = B.convertFromBase B.Base64 (unbase64P b)

encodeBase64P :: ByteString -> Base64PByteString
encodeBase64P = encodeBase

-- JSON

instance FromJSON Base64PByteString where
  parseJSON v = do
    t :: Text <- parseJSON v
    let bs = encodeUtf8 t
    case parseEncodedBS bs of
      Left err -> fail $ show err
      Right baseBS -> pure baseBS

instance ToJSON Base64PByteString where
  toJSON = toJSON . decodeUtf8 . unbase

-------------------------------------------------------------------------------
-- ByteString
-------------------------------------------------------------------------------

instance ByteStringEncoding ByteString where
  encodeBase = identity
  decodeBase = identity
  unbase = identity
  parseEncodedBS = pure
