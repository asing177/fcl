{-|

Cryptography.

-}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Language.FCL.Key (
  -- ** Key types
  PubKey(..),
  ECDSA.PrivateKey,
  ECDSA.Signature,
  ECDSAKeyPair,

  -- ** Generation
  new,
  new',
  newPub,
  newPub',
  newPriv,

  -- ** Signing
  sign,
  signS,
  signWith,
  signSWith,
  signGen,
  verify,
  InvalidSignature(..),

  -- ** Recovery
  recover,

  -- ** HMAC
  hmacSha256,

  -- ** Curves and Ciphers
  sec_p256k1,
  aes256,
  desEde3,

  validateKey,
  validatePair,
  extractPoint,
  fromPoint,
  toPublic,
  fromSecret,

  -- ** Serialization
  encodeKey,
  decodeKey,

  HexPub(..),
  HexPriv(..),

  -- ** Hex Encoding
  encodeHexPriv,
  encodeHexPub,
  unHexPub,
  unHexPriv,
  decodeHexPriv,
  decodeHexPub,
  tryDecodePub,
  tryDecodePriv,

  -- ** Fingerprinting
  fingerprint,

  -- ** PEM and x509
  importPriv,
  importPub,

  encodePriv,
  decodePriv,

  -- ** Export/Import

  readKeys,
  safeWritePubKey,
  safeWritePubKey',
  safeWritePrivKey,
  safeWritePrivKey',

  exportPriv,
  exportPub,

  pemData,
  pubKeyToAsn,
  encodeDer,
  decodeDer,

  compressPair,
  uncompressPair,
  compressPoint,
  uncompressPoint,

  encodeSig,
  putSignature,
  decodeSig,
  decodeSig',
  getSignature,

  getSignatureRS,
  mkSignatureRS,

  -- ** Diffie-Hellman
  DH.SharedKey(..),
  dhGenerateKeyPair,
  dhGetShared,
  dhSecret,
  dhEncode,
  secretToPrivate,

  -- ** Encryption
  encrypt,
  decrypt,

) where

import Protolude hiding (get, put)

import Control.Monad.Fail

import qualified Language.FCL.Encoding as Encoding
import qualified Language.FCL.Utils as Utils

import Data.Aeson (ToJSON(..), FromJSON(..), Value(String))
import Data.Aeson.Types (typeMismatch)
import Data.ByteArray as B
import qualified Data.PEM as PEM -- pem package
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Binary as BI
import qualified Data.Serialize as S

import Data.ASN1.Types
import Data.ASN1.Encoding
import Data.ASN1.BitArray
import Data.ASN1.BinaryEncoding
import qualified Data.X509 as X509

import Crypto.Hash
import Crypto.Random.Types (MonadRandom(..))
import Crypto.Number.Serialize

import Crypto.Error
import Crypto.Data.Padding (Format(..), pad, unpad)

import qualified Crypto.MAC.HMAC as HM
import qualified Crypto.PubKey.ECC.DH as DH

import Crypto.PubKey.ECC.Types
import Crypto.PubKey.ECC.Generate
import Crypto.Number.Generate (generateBetween)
import qualified Crypto.PubKey.ECC.Prim as ECC
import qualified Crypto.PubKey.ECC.Types as ECC
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA

import Crypto.Number.ModArithmetic (inverse)
import Math.NumberTheory.Moduli.Sqrt (sqrtsModPrime)
import Math.NumberTheory.UniqueFactorisation (isPrime)

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.TripleDES (DES_EDE3)
import Crypto.Cipher.Types (Cipher(..), BlockCipher(..), IV, makeIV, blockSize)

import System.Directory (doesFileExist)
import System.Posix.Files (setFileMode, ownerReadMode)

import Test.QuickCheck hiding (generate)
import qualified Language.FCL.Hash as Hash

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

-- | A serializable ECC public key
-- An ECC pubkey is created by multiplying a generator for the group
-- G with the secret key x:
--
-- > Pub = x G
--
-- The result is serialized as a 33-byte array.
--
-- ECC public keys obey the additively homomorphic property:
--
-- > Pub1 + Pub2 = (x1 + x2 (mod n)) G
newtype PubKey = PubKey ECDSA.PublicKey
  deriving (Show, Eq, Generic)

instance Arbitrary PubKey where
  arbitrary = arbitrary >>= \(Positive d) ->
    pure $ fst (new' d)

instance Arbitrary ECDSA.PrivateKey where
  arbitrary = arbitrary >>= \(Positive d) ->
    pure $ snd (new' d)

instance Hash.Hashable PubKey where
  toHash  = Hash.toHash . extractPoint

instance ToJSON PubKey where
   toJSON = String . decodeUtf8 . unHexPub . encodeHexPub

instance FromJSON PubKey where
  parseJSON (String pk) =
    case Encoding.parseEncodedBS (encodeUtf8 pk) of
      Left (Encoding.BadEncoding err) -> fail (show err)
      Right b -> case decodeHexPub (HexPub b) of
        Left err     -> fail $ "PubKey" <> err
        Right pubKey -> pure pubKey
  parseJSON invalid = typeMismatch "PubKey" invalid

type ECDSAKeyPair = (PubKey, ECDSA.PrivateKey)

instance NFData ECDSA.PublicKey where rnf !_ = ()
instance NFData ECDSA.PrivateKey where rnf !_ = ()
instance NFData ECDSA.Signature where rnf !_ = ()

-- | (pk, sk) <- new
-- Returns a new elliptic curve key pair.
--
-- WARNING: Vulnerable to timing attacks.
new :: MonadRandom m => m (PubKey, ECDSA.PrivateKey)
new = do
  (pk, sk) <- generate sec_p256k1
  return (PubKey pk, sk)

-- | Deterministic version of `new`
new' :: Integer -> (PubKey, ECDSA.PrivateKey)
new' d = (PubKey pk, sk)
  where
    curve = sec_p256k1
    q = generateQ curve d
    (pk, sk) = (ECDSA.PublicKey curve q, ECDSA.PrivateKey curve d)

-- | pk <- new
--
-- WARNING: Vulnerable to timing attacks.
newPub :: MonadRandom m => m PubKey
newPub = fst <$> new

newPub' :: Integer -> PubKey
newPub' = fst . new'

-- | sk <- new
--
-- WARNING: Vulnerable to timing attacks.
newPriv :: MonadRandom m => m ECDSA.PrivateKey
newPriv = snd <$> new

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- | Orphan Ord instance for using Signature in a Set
instance Ord ECDSA.Signature where
  s1 `compare` s2 = getSignatureRS s1 `compare` getSignatureRS s2

-- | Sign a message with a private key with ECDSA
--
-- > sig <- sign(sk, m)
--
-- Not deterministic, uses random nonce generation.
-- WARNING: Vulnerable to timing attacks.
sign
  :: MonadRandom m
  => ECDSA.PrivateKey
  -> ByteString
  -> m ECDSA.Signature
sign priv msg = do
  k <- generateBetween 1 (n - 1)
  case signWith priv k msg of
    Nothing -> sign priv msg

    Just sig -> return sig
  where
    n = ECC.ecc_n (ECC.common_curve sec_p256k1)

-- | Generate signature in the Gen monad
signGen
  :: ECDSA.PrivateKey
  -> ByteString
  -> Gen ECDSA.Signature
signGen priv msg = do
  k <- arbitrary `suchThat` (\x -> x >= 1 && x <= (n - 1))
  if validKeypairKPrecondition priv k msg
    then signGen priv msg
    else case ECDSA.signWith k priv SHA3_256 msg of
           Nothing -> signGen priv msg
           Just sig -> pure sig
  where
    n = ECC.ecc_n (ECC.common_curve sec_p256k1)

-- Deterministic, uses explicit nonce.
-- WARNING: Vulnerable to timing attacks.
signWith
  :: ECDSA.PrivateKey
  -> Integer
  -> ByteString
  -> Maybe ECDSA.Signature
signWith priv k msg
  | validKeypairKPrecondition priv k msg = signWith_ priv k msg
  | otherwise = Nothing

signWith_
  :: ECDSA.PrivateKey
  -> Integer
  -> ByteString
  -> Maybe ECDSA.Signature
signWith_ priv k msg = ECDSA.signWith k priv SHA3_256 msg

validKeypairKPrecondition :: ECDSA.PrivateKey -> Integer -> ByteString -> Bool
validKeypairKPrecondition priv k msg =
  let pub  = toPublic priv
      sigM = signWith_ priv k msg
  in case sigM of
      Nothing ->
        False

      Just sig ->
        let (c1, c2) = recover_ sig msg
        in if | c1 == pub -> True
              | c2 == pub -> True
              | True      -> False

-- | Verify a message with a public key with ECDSA
--
-- > 0/1 <- verify(pk, m, sig)
verify
  :: PubKey
  -> ECDSA.Signature
  -> ByteString
  -> Bool
verify (PubKey key) sig msg =
  ECDSA.verify SHA3_256 key sig msg

-- | Sign a serializable instance encoding in binary with ECDSA.
--
-- Not deterministic, uses random nonce generation.
-- WARNING: Vulnerable to timing attacks.
signS
  :: (S.Serialize a, MonadRandom m)
  => ECDSA.PrivateKey
  -> a
  -> m ECDSA.Signature
signS priv msg = sign priv (S.encode msg)

-- | Sign a serializable instance encoding in binary with ECDSA.
--
-- Deterministic, uses explicit nonce.
-- WARNING: Vulnerable to timing attacks.
signSWith
  :: S.Serialize a
  => ECDSA.PrivateKey
  -> Integer
  -> a
  -> Maybe ECDSA.Signature
signSWith priv k msg = signWith priv k (S.encode msg)

-------------------------------------------------------------------------------
-- HMAC
-------------------------------------------------------------------------------

hmacSha256 :: ByteString -> ByteString -> ByteString
hmacSha256 key msg = B.convert (mac key msg)
  where
    mac :: ByteString -> ByteString -> HM.HMAC SHA3_256
    mac = HM.hmac

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

-- | The 256-bit Koblitz elliptic curve over F_p with and verifiably random
-- parameters.
--
-- Reference: http://www.secg.org/SEC2-Ver-1.0.pdf#page=21
--
-- > p   = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f
-- > a   = 0x0000000000000000000000000000000000000000000000000000000000000000
-- > b   = 0x0000000000000000000000000000000000000000000000000000000000000007
-- > g_x = 0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798
-- > g_y = 0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8
-- > n   = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141
-- > h   = 0x1
sec_p256k1 :: Curve
sec_p256k1 = ECC.getCurveByName SEC_p256k1

pointSize :: ECC.Curve -> Int
pointSize = toBytes . ECC.curveSizeBits
  where
    toBytes bits = (bits + 7) `div` 8

instance S.Serialize PubKey where
  put (PubKey key) = S.put (exportKey key)
  get = S.get >>= loadKey

encodePriv :: ECDSA.PrivateKey -> ByteString
encodePriv key = S.encode (ECDSA.private_d key)

decodePriv :: ByteString -> Either [Char] ECDSA.PrivateKey
decodePriv bs = S.decode bs >>= pure . fromSecret

-- | Export the elliptic curve public key
exportKey :: ECDSA.PublicKey -> (Int, Integer, Integer)
exportKey key = (curve,x,y)
  where
    curve         = fromEnum SEC_p256k1
    ECC.Point x y = ECDSA.public_q key

-- | Validate a public key as a valid secp25k1 ECC key
validateKey :: PubKey -> Bool
validateKey (PubKey key) = ECC.isPointValid sec_p256k1 point
  where
    point = ECDSA.public_q key

-- | Validate that a public-private key pair are derived from each other.
validatePair :: ECDSAKeyPair -> Bool
validatePair (pub, priv) = toPublic priv == pub

-- | Export the elliptic curve point for a public key.
extractPoint :: PubKey -> (Integer, Integer)
extractPoint (PubKey key) = (x,y)
  where
    ECC.Point x y = ECDSA.public_q key

-- | Import the elliptic curve point for a public key. ( Not safe )
fromPoint :: (Integer, Integer) -> PubKey
fromPoint (x,y)
    | ECC.isPointValid sec_p256k1 p = PubKey $ ECDSA.PublicKey sec_p256k1 (ECC.Point x y)
    | otherwise                     = panic "Invalid elliptic curve point"
  where
    p = ECC.Point x y

-- | Create a public key from a secret key
--
-- WARNING: Vulnerable to timing attacks.
toPublic :: ECDSA.PrivateKey -> PubKey
toPublic key = PubKey (ECDSA.PublicKey curve point)
  where
    curve  = ECDSA.private_curve key
    curve' = ECC.common_curve curve
    point  = ECC.pointMul curve (ECDSA.private_d key) g
    g      = ECC.ecc_g curve'

-- | Import the elliptic curve secret for a private key. ( UNSAFE )
fromSecret :: Integer -> ECDSA.PrivateKey
fromSecret d =
  ECDSA.PrivateKey
    { ECDSA.private_curve = sec_p256k1
    , private_d = d
    }

-- | Deserialize a public key encoded with the curve
loadKey
  :: (Int, Integer, Integer)
  -> S.Get PubKey
loadKey (curve,x,y) = do
  key <- ECDSA.PublicKey <$> curve' <*> pure point
  if ECC.isPointValid sec_p256k1 point
    then return (PubKey key)
    else mzero -- Fail to parse invalid point
  where
    point  = ECC.Point x y
    curve' = pure (ECC.getCurveByName (toEnum curve))

-- | Extract (r,s) parameters of a digital signature ( UNSAFE )
getSignatureRS :: ECDSA.Signature -> (Integer,Integer)
getSignatureRS (ECDSA.Signature r s) = (r,s)

-- | Transform (r,s) into `ECDSA.Signature r s` ( UNSAFE )
mkSignatureRS :: (Integer, Integer) -> ECDSA.Signature
mkSignatureRS (r,s) = ECDSA.Signature r s -- XXX position is invalid; function only used in Script/Eval.hs so far


-------------------------------------------------------------------------------
-- Export / Import
-------------------------------------------------------------------------------

-- | Reads a private key found at the given path
readKeys :: FilePath -> IO (Either Text ECDSAKeyPair)
readKeys privKeyPath =
  join . fmap importPriv <$> Utils.safeRead privKeyPath

-- | Writes a private key to a given file in PEM format
-- Note: Does not overwrite existing files
safeWritePrivKey :: FilePath -> ECDSA.PrivateKey -> IO (Either Text ())
safeWritePrivKey file privKey = do
  privExists <- doesFileExist file
  if privExists
    then pure $ Left $
      "Not overwriting existing Private Key at: " <> toS file
    else safeWritePrivKey' file privKey

-- | Writes a private key to a given file in PEM format
-- Note: Overwrites existing file
safeWritePrivKey' :: FilePath -> ECDSA.PrivateKey -> IO (Either Text ())
safeWritePrivKey' file privKey = do
  eRes <- Utils.safeWrite file $ exportPriv privKey
  case eRes of
    Left err -> pure $ Left err -- V Set chmod 0400
    Right _  -> Right <$> setFileMode file ownerReadMode

--------------------------------------------------------------------------------

-- | Write a PEM serialized Public Key to disk
-- Note: Does not overwrite existing file at the given filepath
safeWritePubKey :: FilePath -> PubKey -> IO (Either Text ())
safeWritePubKey file pubKey = do
  privExists <- doesFileExist file
  if privExists
    then pure $ Left $
      "Not overwriting existing Public Key at: " <> toS file
    else safeWritePubKey' file pubKey

-- | Write a PEM serialized Public Key to disk
-- Warning: Overwrites existing file at the given filepath
safeWritePubKey' :: FilePath -> PubKey -> IO (Either Text ())
safeWritePubKey' file pubKey = do
  eRes <- Utils.safeWrite file $ exportPub pubKey
  case eRes of
    Left err -> pure $ Left err -- V Set chmod 0400
    Right _  -> Right <$> setFileMode file ownerReadMode

-------------------------------------------------------------------------------

ecOid :: ASN1
ecOid = OID (getObjectID X509.PubKeyALG_EC)

secp256k1Oid :: ASN1
secp256k1Oid = OID [1,3,132,0,10]

ecKey :: X509.SerializedPoint -> X509.PubKeyEC
ecKey = X509.PubKeyEC_Named SEC_p256k1

encodeEc :: X509.SerializedPoint -> ASN1S
encodeEc point = toASN1 (X509.PubKeyEC (ecKey point))

serializePoint :: CurveName -> (Integer, Integer) -> X509.SerializedPoint
serializePoint name (x,y) = pub
  where
    pub   = X509.SerializedPoint bs
    bs    = B.cons 4 (i2ospOf_ bytes x `B.append` i2ospOf_ bytes y)
    bits  = ECC.curveSizeBits (ECC.getCurveByName name)
    bytes = (bits + 7) `div` 8

pubKeyToAsn :: PubKey -> [ASN1]
pubKeyToAsn pub = (encodeEc $ serializePoint SEC_p256k1 (extractPoint pub)) []

uncompressPair :: [ASN1] -> (Integer, (Integer, Integer))
uncompressPair xs =
  case xs of
    [ Start Sequence
      , IntVal 1
      , OctetString priv
      , Start (Container Context 0)
      , secp256k1Oid
      , End (Container Context 0)
      , Start (Container Context 1)
      , BitString (BitArray 520 pub)
      , End  (Container Context 1)
      , End Sequence
      ] ->
        let (x, y) = BS.splitAt 32 (BS.drop 1 pub) in
        (os2ip priv, (os2ip x, os2ip y))
    _ -> panic "Invalid point compression."

compressPair :: (Integer, Integer) -> Integer -> [ASN1]
compressPair (x,y) priv =
  [ Start Sequence
  , IntVal 1
  , OctetString (i2osp priv)
  , Start (Container Context 0)
  , secp256k1Oid
  , End (Container Context 0)
  , Start (Container Context 1)
  , BitString (BitArray 520 pubBits)
  , End  (Container Context 1)
  , End Sequence
  ]
  where
    pubBits = BS.cons 4 (i2osp x <> i2osp y)

uncompressPoint :: [ASN1] -> (Integer, Integer)
uncompressPoint xs =
  case xs of
    [ Start Sequence
      , Start Sequence
      , ecOid
      , secp256k1Oid
      , End Sequence
      , BitString (BitArray 520 pub)
      , End Sequence
      ] ->
        let (x, y) = BS.splitAt 32 (BS.drop 1 pub) in
        (os2ip x, os2ip y)
    _ -> panic "Invalid point compression."

compressPoint :: (Integer, Integer) -> [ASN1]
compressPoint (x,y) =
  [ Start Sequence
    , Start Sequence
    , ecOid
    , secp256k1Oid
    , End Sequence
    , BitString (BitArray 520 pubBits)
    , End Sequence
    ]
  where
    pubBits = BS.cons 4 (i2osp x <> i2osp y)

-- | Import public/private keypair from PEM file
importPriv :: ByteString -> Either Text ECDSAKeyPair
importPriv bs =
  if validateKey point
    then Right (point, priv)
    else Left "Invalid key data. Is not elliptic curve point"
  where
    pem = pemData bs
    Right der = decodeDer pem
    (d, (x0, x1)) = uncompressPair der
    point = fromPoint (x0, x1)
    priv = fromSecret d

-- | Import public key from PEM file
importPub :: ByteString -> Either Text PubKey
importPub bs =
  case decodeDer (pemData bs) of
    Left err -> Left "Invalid PEM format"
    Right der -> do
      let (x0, x1) = uncompressPoint der
      let point = fromPoint (x0, x1)
      if validateKey point
        then Right point
        else Left "Invalid key data. Is not elliptic curve point"

-- | Export public/private keypair to PEM file
exportPriv :: ECDSA.PrivateKey -> ByteString
exportPriv priv = do
  let (x0, x1) = extractPoint $ toPublic priv
  let asn1 = encodeDer $ compressPair (x0, x1) (ECDSA.private_d priv)
  pemPrivate asn1

-- | Export public key to PEM file
exportPub :: PubKey -> ByteString
exportPub pub = do
  let (x0, x1) = extractPoint pub
  let asn1 = encodeDer $ compressPoint (x0, x1)
  pemPublic asn1

-------------------------------------------------------------------------------
-- Binary Encoding
-------------------------------------------------------------------------------

-- | Binary serialize public key ( UNSAFE )
encodeKey :: PubKey -> ByteString
encodeKey = S.encode

-- | Binary deserialize public key ( UNSAFE )
decodeKey :: ByteString -> Either [Char] PubKey
decodeKey = S.decode

{- Serialization Note:

   Binary encoding of signatures tightly reflects the binary encoding of
   signatures that are serialized and sent from the SDKs. Modifying this code
   and/or putSafeString & getSafeString may cause deserialization of signatures
   sent from the SDKs to fail.

-}

putSignature :: S.Putter ECDSA.Signature
putSignature (ECDSA.Signature r s) = do
    S.put $ encInteger r
    S.put ':'
    S.put $ encInteger s
  where
    encInteger :: Integer -> ByteString
    encInteger = show

getSignature :: S.Get ECDSA.Signature
getSignature = do
  rBS <- S.get
  _ <- S.get :: S.Get Char
  sBS <- S.get
  let read' = head . reads . BSC.unpack
  let mRS = do
        r' <- read' rBS
        s' <- read' sBS
        return (fst r', fst s')
  case mRS of
    Nothing -> fail "Could not decode ECDSA.signature"
    Just (r,s) -> return $ ECDSA.Signature r s

data InvalidSignature
  = InvalidSignature ECDSA.Signature ByteString
  | DecodeSignatureFail ByteString
  | SignatureSplittingFail ByteString
  deriving (Show, Eq, Generic, S.Serialize)

instance Arbitrary InvalidSignature where
  arbitrary = oneof
    [ InvalidSignature <$> (ECDSA.Signature <$> arbitrary <*> arbitrary) <*> (toS <$> arbitrary @Text)
    , DecodeSignatureFail <$> (toS <$> arbitrary @Text)
    , SignatureSplittingFail <$> (toS <$> arbitrary @Text)
    ]
-- XXX Wrap ECDSA.Signature in newtype to prevent orphan instances
instance S.Serialize ECDSA.Signature where
  put = S.put . encodeSig
  get = do
    sigBS <- decodeSig <$> S.get
    case sigBS of
      Left err -> fail $ show err
      Right sig -> pure sig

instance BI.Binary ECDSA.Signature where
  put = Utils.putBinaryViaSerialize
  get = Utils.getBinaryViaSerialize

-- | Binary encoding of a signature
encodeSig :: ECDSA.Signature -> Encoding.Base64PByteString
encodeSig = Encoding.encodeBase . S.runPut . putSignature

-- Binary decoding of a signature
decodeSig :: Encoding.Base64PByteString -> Either InvalidSignature ECDSA.Signature
decodeSig bs =
  first (DecodeSignatureFail . toS) $ S.runGet getSignature (Encoding.decodeBase bs)

decodeSig' :: Encoding.Base64PByteString -> ECDSA.Signature
decodeSig' bs =
  case decodeSig bs of
    Left err -> panic $ show err
    Right sig -> sig

-------------------------------------------------------------------------------
-- Hex Encoding
-------------------------------------------------------------------------------

-- | Hexadecimal encoded public key
newtype HexPub = HexPub Encoding.Base16ByteString
  deriving (Eq, Ord, Show, Generic, S.Serialize, Hash.Hashable)

-- | Hexadecimal encoded private key
newtype HexPriv = HexPriv Encoding.Base16ByteString
  deriving (Eq, Ord, Show, Generic, S.Serialize, Hash.Hashable)

-- | Convert HexPub to raw ByteString
unHexPub :: HexPub -> ByteString
unHexPub (HexPub b) = Encoding.unbase b

-- | Convert HexPriv to raw ByteString
unHexPriv :: HexPriv -> ByteString
unHexPriv (HexPriv b) = Encoding.unbase b

-- | Hex encoding of prviate key
encodeHexPriv :: ECDSA.PrivateKey -> HexPriv
encodeHexPriv = HexPriv . Encoding.encodeBase . i2osp . ECDSA.private_d

-- | Hex encoding of public key
encodeHexPub :: PubKey -> HexPub
encodeHexPub pub = HexPub (Encoding.encodeBase (i2ospOf_ 32 x <> i2ospOf_ 32 y))
  where
    (x, y) = extractPoint pub

-- | Dehex private key
decodeHexPriv :: HexPriv -> Either [Char] ECDSA.PrivateKey
decodeHexPriv (HexPriv b) = do
  bs <- Encoding.decodeBase16E b
  let bs' = if odd (BS.length bs)
         then "0" <> bs
         else bs
  pure $ ECDSA.PrivateKey sec_p256k1 (os2ip bs')
  -- B.convertFromBase B.Base16 only likes even lengthed bytestrings...

-- | Dehex public key
decodeHexPub :: HexPub -> Either [Char] PubKey
decodeHexPub (HexPub b) = do
  bs <- Encoding.decodeBase16E b
  let (xs, ys) = BS.splitAt 32 bs
  let point = ECC.Point (os2ip xs) (os2ip ys)
  if ECC.isPointValid sec_p256k1 point
    then Right (PubKey (ECDSA.PublicKey sec_p256k1 point))
    else Left "dehexPub: Invalid public key point"

tryDecodePub :: ByteString -> Either Text PubKey
tryDecodePub pubBS = case importPub pubBS of
  Left _ -> case Encoding.parseEncodedBS pubBS of
    Left _ -> Left errMsg
    Right b -> first (const errMsg) (decodeHexPub (HexPub b))
  Right pub -> Right pub
  where
    errMsg = "Failed to decode ECDSA public key from hex or pem format."

tryDecodePriv :: ByteString -> Either Text ECDSA.PrivateKey
tryDecodePriv privBS = case importPriv privBS of
    Left _ -> case Encoding.parseEncodedBS privBS of
      Left _ -> Left errMsg
      Right b -> first (const errMsg) (decodeHexPriv (HexPriv b))
    Right (_,priv) -> Right priv
    where
      errMsg = "Failed to decode ECDSA private key from hex or pem format."


-------------------------------------------------------------------------------
-- DER / PEM Encoding
-------------------------------------------------------------------------------

-- | Deocde DER data
decodeDer :: Maybe BS.ByteString -> Either [Char] [ASN1]
decodeDer (Just bs) =
  case decodeASN1' DER bs of
    Left _ -> Left "couldn't decode ASN1 stream"
    Right r -> Right r
decodeDer Nothing = Left "decodeDer: bytestring was Nothing"

-- | Encode DER data
encodeDer :: [ASN1] -> ByteString
encodeDer = encodeASN1' DER

encryptedHeader :: ByteString -> [([Char], ByteString)]
encryptedHeader iv = [
    {-("Proc-Type", "4,ENCRYPTED")-}
  {-, ("DEK-Info", "DES-EDE3-CBC" <> "," <> iv)-}
  ]

-- | Write a private elliptic curve key PEM file
pemPrivate :: ByteString -> ByteString
pemPrivate asn1 = PEM.pemWriteBS pem
  where
    pem =
      PEM.PEM
      { PEM.pemName = "EC PRIVATE KEY"
      {-, PEM.pemHeader = encryptedHeader ""-}
      , PEM.pemHeader = []
      , PEM.pemContent = asn1
      }

-- | Write a public elliptic curve key PEM file
pemPublic :: ByteString -> ByteString
pemPublic asn1 = PEM.pemWriteBS pem
  where
    pem =
      PEM.PEM
      { PEM.pemName = "EC PUBLIC KEY"
      , PEM.pemHeader = []
      , PEM.pemContent = asn1
      }

pemSignature :: [ASN1] -> ByteString
pemSignature asn1 = PEM.pemWriteBS pem
  where
    pem =
      PEM.PEM
      { PEM.pemName = "EC SIGNATURE"
      , PEM.pemHeader = []
      , PEM.pemContent = encodeDer asn1
      }

-- | Extract the body of a PEM datastream by parsing
pemData :: ByteString -> Maybe ByteString
pemData bs =
  case PEM.pemParseBS bs of
    Right [pem] -> Just $ PEM.pemContent pem
    Right _     -> Nothing
    Left  _     -> Nothing

-------------------------------------------------------------------------------
-- Fingerprinting
-------------------------------------------------------------------------------

-- | Produce the SHA256 fingerprint of a public key.
fingerprint :: PubKey -> [Char]
fingerprint pub = intercalate ":" (fmap Utils.showHex bits)
  where
    bits  = Utils.toByteList (Hash.sha256Raw str)
    str   = i2osp x <> i2osp y
    (x,y) = extractPoint pub

-------------------------------------------------------------------------------
-- ECDSA PubKey Recovery
-------------------------------------------------------------------------------

-- ECDSA Key Recovery
--
-- First, you find the two points R, R′ which have the value r as the
-- x-coordinate r.
--
-- You also compute r^{−1}, which is the multiplicative inverse of the value r
-- from the signature (modulo the order of the generator of the curve).
--
-- Then, you compute z which is the lowest n bits of the hash of the message
-- (where n is the bit size of the curve).
--
-- The two public key canidates are then:
--
-- > Q = r^{-1}(sR - zG)
-- > Q = r^{-1}(sR' - zG)
--
-- See: https://github.com/bitcoin-core/secp256k1/blob/master/src/modules/recovery/main_impl.h
-- Reference: http://www.secg.org/sec1-v2.pdf#p=47

-- | Recover a public key from a signature and message hash.
--
-- WARNING: Vulnerable to timing attacks.
recover
  :: ECDSA.Signature
  -> ByteString
  -> (PubKey, PubKey)
recover sig msg = recover_ sig msg

-- Only used in order to determine which recovered key is the correct one
-- in order to preprend the signature with a 0 (first key) or 1 (second key)
recover_
  :: ECDSA.Signature
  -> ByteString
  -> (PubKey, PubKey)
recover_ sig@(ECDSA.Signature r s) msg =
  let hash = os2ip (Hash.sha256Raw msg)
      [x0] = fmap (\x -> mod x p) $ takeWhile (<p) [(r + i*n) | i <- [0..h]]
      Just invr = inverse r n
      Just (y0:_) = sqrtsModPrime (x0^3 + a*x0 + b) <$> isPrime p
      p0 = ECC.Point x0 y0
      q0 = ECC.pointAddTwoMuls sec_p256k1 (invr * s) p0
                                          (invr * (-hash)) g

      c1 = PubKey (ECDSA.PublicKey sec_p256k1 q0)

      p0' = ECC.pointMul sec_p256k1 (-1) p0
      q0' = ECC.pointAddTwoMuls sec_p256k1 (invr * s) p0'
                                           (invr * (-hash)) g
      c2 = (PubKey (ECDSA.PublicKey sec_p256k1 q0'))

  in (c1, c2)

  where
    curve = (ECC.common_curve sec_p256k1)
    ECC.CurveFP pcurve = sec_p256k1
    n = ECC.ecc_n curve
    a = ECC.ecc_a curve
    b = ECC.ecc_b curve
    p = ECC.ecc_p pcurve
    h = ECC.ecc_h curve
    g = ECC.ecc_g curve

-------------------------------------------------------------------------------
-- Diffie-Hellman
-------------------------------------------------------------------------------

-- XXX: redundent type definitions

data ECDHParams = ECDHParams ECC.Curve ECC.CurveName
  deriving (Show,Eq)

dhGenerateKeyPair :: MonadRandom m => m (ECDSA.PrivateKey, PubKey)
dhGenerateKeyPair = do
  secret <- DH.generatePrivate sec_p256k1
  let point = DH.calculatePublic sec_p256k1 secret
      pub   = PubKey (ECDSA.PublicKey sec_p256k1 point)
      priv  = ECDSA.PrivateKey sec_p256k1 secret
  return (priv, pub)

dhGetShared :: ECDSA.PrivateKey -> PubKey -> Maybe DH.SharedKey
dhGetShared priv (PubKey pub)
  | ECC.isPointValid curve point = Just $ DH.getShared curve secret point
  | otherwise                    = Nothing
  where
    curve  = sec_p256k1
    point  = ECDSA.public_q pub
    secret = ECDSA.private_d priv

secretToPrivate :: DH.SharedKey -> ECDSA.PrivateKey
secretToPrivate s1 =
  ECDSA.PrivateKey
    { ECDSA.private_curve = sec_p256k1
    , private_d = os2ip s1 }

dhSecret :: ECDSA.PrivateKey -> DH.SharedKey
dhSecret priv = DH.getShared sec_p256k1 (ECDSA.private_d priv) point
  where
    point = DH.calculatePublic sec_p256k1 (ECDSA.private_d priv)

dhEncode :: ECDSA.PrivateKey -> Encoding.Base16ByteString
dhEncode = Encoding.encodeBase . B.convert . dhSecret

-------------------------------------------------------------------------------
-- Block Ciphers
-------------------------------------------------------------------------------

blockLength :: Int
blockLength = blockSize aes256

-- | AES Cipher proxy
aes256 :: AES256
aes256 = witness

-- | 3DES Cipher proxy
desEde3 :: DES_EDE3
desEde3 = witness

cipherInitNoErr :: BlockCipher c => ScrubbedBytes -> c
cipherInitNoErr k = case cipherInit k of
  CryptoPassed a -> a
  -- If we can't initialize cipher, something is wrong upstream so abort.
  CryptoFailed e -> panic (show e)

prepareKey :: ScrubbedBytes -> ScrubbedBytes
prepareKey key =
  if B.length key == 16 || B.length key == 32
    then key
    else pad (PKCS7 blockLength) key

-- | Draw entropy from random monad for cipher initialization
makeEntropy :: MonadRandom m => Int -> m ByteString
makeEntropy = getRandomBytes

-- | Encrypt using AES256-CBC using computed elliptic curve shared secret of
-- the underlying private key, computed using ECDH.
-- The key must be less than 32 bytes in length
--
-- > encrypt(key, msg)
encrypt :: MonadRandom m => ScrubbedBytes -> ByteString -> m ByteString
encrypt key msg = do
  bs <- makeEntropy blockLength
  let Just iv = makeIV bs :: Maybe (IV AES256)
  let key' = prepareKey key
  let msg' = pad (PKCS7 blockLength) msg
  let ctx = cipherInitNoErr key'
  return $ bs <> (cbcEncrypt ctx iv msg')

-- | Decrypt using AES256-CBC
--
-- > decrypt(key, msg)
decrypt :: ScrubbedBytes -> ByteString -> Maybe ByteString
decrypt key msg =
  let (iv, msg') = B.splitAt blockLength msg
      Just iv' = makeIV iv :: Maybe (IV AES256)
      key' = prepareKey key
      ctx = cipherInitNoErr key'
      msg'' = cbcDecrypt ctx iv' msg'
  in unpad (PKCS7 blockLength) msg''
