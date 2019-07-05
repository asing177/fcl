{-|

Test fixtures.

-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Reference where

import Protolude

import Test.QuickCheck
import Unsafe (unsafeFromJust)
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (catch)

import qualified Crypto.PubKey.ECC.Types as ECC
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import qualified Crypto.PubKey.ECC.DH as DH
import qualified Crypto.Number.Serialize as CNS
import Crypto.Random.Types (MonadRandom(..))

import qualified Data.ByteArray as B
import qualified Data.Set as Set
import qualified Data.Map as Map
import Text.Read (read)

import Language.FCL.Address as Address
import Language.FCL.Asset as Asset
import Language.FCL.Metadata as Metadata
import Language.FCL.Contract as Contract
import qualified Language.FCL.World as World
import Language.FCL.World (Account', Asset')
import Language.FCL.Hash as Hash
import Language.FCL.Encoding as Encoding
import Numeric.Lossless.Number (Number(..))
import Language.FCL.Storage as Storage
import Language.FCL.AST
import Language.FCL.Eval as Eval
import Language.FCL.Parser (parseFile)
import qualified Language.FCL.Key as Key
import Language.FCL.Time as Time
import qualified Language.FCL.Delta as Delta
import qualified Language.FCL.SafeString as SafeString
import qualified Language.FCL.Prim as Prim

-------------------------------------------------------------------------------
-- Timestamp Fixtures
-------------------------------------------------------------------------------

testTimestamp :: Timestamp
testTimestamp = 1231006505

{-# NOINLINE unsafeTimestamp #-}
unsafeTimestamp :: Timestamp
unsafeTimestamp = unsafePerformIO Time.now

-------------------------------------------------------------------------------
-- Address Fixtures
-------------------------------------------------------------------------------

deriveHash :: ByteString -> Hash.Hash Encoding.Base58ByteString
deriveHash = Hash.sha256 . Hash.sha256Raw . Hash.ripemd160Raw . Hash.sha256Raw

deriveAddress :: Key.PubKey -> Address a
deriveAddress pub = Address (Hash.getRawHash $ deriveHash pstr)
  where
    (x, y) = Key.extractPoint pub
    pstr = (show x) <> (show y)

recoverAddress :: Key.Signature -> ByteString -> (Address a, Address a)
recoverAddress sig = bimap deriveAddress deriveAddress . Key.recover sig

newAddr :: MonadRandom m => m (Address a)
newAddr = Key.new >>= \(pub, priv) -> pure (deriveAddress pub)

testAddr :: Address a
testAddr = deriveAddress testPub

testAddr2 :: Address a
testAddr2 = deriveAddress testPub2

testAddr3 :: Address a
testAddr3 = deriveAddress testPub3

testHolder :: Holder
testHolder = AccountHolder (testAddr :: Address AAccount)

-------------------------------------------------------------------------------
-- Transaction Header Fixtures
-------------------------------------------------------------------------------

-- XXX Make test tx headers for all tx header types

assetAddr_, toAddr_ :: Address a
assetAddr_ = Address "43WRxMNcnYgZFcE36iohqrXKQdajUdAxeSn9mzE1ZedB"
toAddr_ = Address "7mR5d7s6cKB4qjuX1kiwwNtygfURhFQ9TKvEd9kmq6QL"

toHolder_ :: Holder
toHolder_ = AccountHolder (toAddr_ :: Address AAccount)

-------------------------------------------------------------------------------
-- Asset Fixtures
-------------------------------------------------------------------------------

data Asset = Asset
  { name      :: Text              -- ^ Name of asset
  , issuer    :: Address AAccount  -- ^ Issuer
  , issuedOn  :: Timestamp         -- ^ Timestamp
  , supply    :: Balance           -- ^ Total supply
  , holdings  :: Holdings          -- ^ Holdings map
  , reference :: Maybe Ref         -- ^ Reference unit
  , atype     :: AssetType         -- ^ Asset type
  , asAddress   :: Address AAsset    -- ^ Asset address
  , metadata  :: Metadata.Metadata -- ^ Asset address
  } deriving (Show, Eq)

newtype Holdings = Holdings { unHoldings :: Map.Map Holder Balance }
  deriving (Eq, Ord, Show)

data Ref
  = USD               -- ^ US Dollars
  | GBP               -- ^ British Pounds
  | EUR               -- ^ Euros
  | CHF               -- ^ Swiss Francs
  | Token             -- ^ Abstract token
  | Security          -- ^ Security
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

instance Arbitrary Reference.Ref where
  arbitrary = elements [ minBound.. maxBound ]

-------------------------------------------------------------------------------
-- Account
-------------------------------------------------------------------------------

data Account = Account
  { acPk        :: Key.PubKey
  , acAddress   :: Address AAccount
  , timezone    :: SafeString.SafeString
  , metadata    :: Metadata
  } deriving (Show, Eq)

testTimezone :: ByteString
testTimezone = "GMT"

testAccount :: Account
testAccount = Account
  { acPk   = testPub
  , acAddress     = deriveAddress testPub
  , timezone    = "America/New_York"
  , metadata    = Metadata $
      Map.fromList [ ("Company", "Adjoint Inc.") ]
  }

testAccount2 :: Account
testAccount2 = Account
  { acPk   = testPub2
  , acAddress     = deriveAddress testPub2
  , timezone    = "America/Boston"
  , metadata    = testMetadata
  }

-------------------------------------------------------------------------------
-- Metadata
-------------------------------------------------------------------------------
testMetadata :: Metadata
testMetadata = Metadata $
  Map.fromList [ ("Company", "Adjoint Inc.") ]


-------------------------------------------------------------------------------
-- Contract
-------------------------------------------------------------------------------

testContract :: Time.Timestamp -> Contract
testContract now = Contract {
    timestamp     = now
  , script        = testScript
  , globalStorage = testGlobalStorage
  , methods       = unLoc <$> methodNames testScript
  , state         = startState
  , address       = testAddr
  , owner         = testAddr
  }

------------------------------------------------------------------------------------
-- Storage
-------------------------------------------------------------------------------

-- TODO All the the Value types
testStorage :: Storage
testStorage = Map.fromList [
    ("a", VNum $ NumDecimal 3)
  , ("b", VNum . NumRational . fromRational $ 22 % 7)
  , ("c", VBool True)
  , ("d", VAccount testAddr)
  , ("e", VVoid)
  , ("g", VConstr "Foo" [VNum 0, VNum 1])
  , ("h", VAsset testAddr2)
  , ("i", VContract testAddr)
  ]

testGlobalStorage :: GlobalStorage
testGlobalStorage = GlobalStorage testStorage

---------------------------------------------------
-- World
---------------------------------------------------

data World = World
  { contracts :: Map.Map (Address AContract) (Contract)
  , assets    :: Map.Map (Address AAsset) Asset
  , accounts  :: Map.Map (Address AAccount) Account
  } deriving (Show, Eq)


data AssetError
  = InsufficientHoldings (Address AAsset) Balance
  | InsufficientSupply (Address AAsset) Balance
  | CirculatorIsNotIssuer Holder (Address AAsset)
  | AssetError
  | SelfTransfer Holder
  | HolderDoesNotExist Holder
  | AssetDoesNotExist (Address AAsset)
  | SenderDoesNotExist Holder
  | ReceiverDoesNotExist Holder
  deriving (Show, Eq)

data AccountError
  = AccountDoesNotExist (Address AAccount)
  deriving (Show, Eq)

-- | Transfer an amount of the asset supply to an account's holdings
circulateSupply :: Holder -> Balance -> Asset -> Either AssetError Asset
circulateSupply addr bal asset
  | supply asset >= bal
    = Right $ asset { holdings = holdings', supply = supply' }
  | otherwise
    = Left $ InsufficientSupply (asAddress asset) (supply asset)
  where
    holdings' = Holdings $ clearZeroes $
      Map.insertWith (+) addr bal $ unHoldings (holdings asset)
    supply' = supply asset - bal
    clearZeroes = Map.filter (/= 0)

transferHoldings :: Holder -> Holder -> Balance -> Asset -> World -> Either AssetError Asset
transferHoldings from to amount asset world
    | from == to = Left $ SelfTransfer from
    | otherwise  =
        case World.assetBalance @World asset from of
          Nothing ->
            Left $ HolderDoesNotExist from
          Just bal
            | amount <= bal -> do
                asset' <- circulateSupply from (negate amount) asset
                circulateSupply to amount asset'
            | otherwise     ->
                Left $ InsufficientHoldings (World.assetToAddr @World asset) bal
    where
      assetIssuer = issuer asset

instance World.World World where
  type Account' World = Account
  type Asset' World = Asset
  type AccountError' World = AccountError
  type AssetError' World = AssetError

  transferAsset assetAddr from to balance world = do
    -- validateTransferAddrs world from to
    case World.lookupAsset assetAddr world of
      Left err -> Left $ AssetDoesNotExist assetAddr
      Right asset -> do
        asset' <- transferHoldings from to balance asset world
        Right $ world { assets = Map.insert assetAddr asset' (assets world) }
      
  circulateAsset assetAddr txOrigin amount world =
    case World.lookupAsset assetAddr world of
    Left err    -> Left $ AssetDoesNotExist assetAddr
    Right asset -> do
      let assetIssuer = issuer asset
      if assetIssuer /= txOrigin
        then Left $ CirculatorIsNotIssuer (AccountHolder txOrigin) (World.assetToAddr @World asset)
        else do
          asset' <- circulateSupply (AccountHolder assetIssuer) amount asset
          Right $ world { assets = Map.insert assetAddr asset' (assets world) }

  lookupContract addr world =
    case Map.lookup addr (contracts world) of
      Nothing -> Left $ World.ContractDoesNotExist addr
      Just contract -> Right contract

  lookupAsset addr world =
    case Map.lookup addr (assets world) of
      Nothing -> Left $ AssetDoesNotExist addr
      Just asset -> Right asset

  lookupAccount addr world =
    case Map.lookup addr (accounts world) of
      Nothing   -> Left $ AccountDoesNotExist addr
      Just acc  -> Right acc

  assetType asset
    = atype asset

  assetBalance asset holder
    = Map.lookup holder (unHoldings $ holdings asset)

  assetToAddr asset
    = asAddress asset

  publicKey account = acPk account

  accountToAddr account = acAddress account

genesisWorld :: World
genesisWorld = World mempty mempty mempty

-------------------------------------------------------------------------------
-- Script
-------------------------------------------------------------------------------

testScript :: Script
testScript = unsafePerformIO (parseFile "tests/script/positive/typecheck/reference.s")

-------------------------------------------------------------------------------
-- Key
-------------------------------------------------------------------------------

testSigDecode :: IO Bool
testSigDecode = do
  (pk1, sk1) <- Key.new
  sig <- Key.sign sk1 "foo"
  let sigE = Key.encodeSig sig
  let Right sigD = Key.decodeSig sigE
  return $ sig == sigD

testDH :: IO (Key.PubKey, Key.PubKey, Bool)
testDH = do
  (priv1, pub1) <- Key.dhGenerateKeyPair -- client
  (priv2, pub2) <- Key.dhGenerateKeyPair -- server
  let Just s1 = Key.dhGetShared priv1 pub2
  let Just s2 = Key.dhGetShared priv2 pub1

  let sec1 = Key.secretToPrivate s1
  let sec2 = Key.secretToPrivate s2
  print (sec1, sec2)

  return (pub1, pub2, sec1 == sec2)

testRecover :: IO Bool
testRecover = do
  let k    = 101
  let Just sig = Key.signWith testPriv k testMsg
  let (rpub1, rpub2) = Key.recover sig testMsg
  return $ testPub == rpub1 || testPub == rpub2

testMsg :: ByteString
testMsg = "The quick brown fox jumped over the lazy dog."

testEncrypt  :: IO ByteString
testEncrypt = do
  let Just (DH.SharedKey key) = Key.dhGetShared testPriv testPub2
  print (B.unpack key)
  Key.encrypt key testMsg

testDecrypt :: IO (Maybe ByteString)
testDecrypt = do
  let Just (DH.SharedKey key) = Key.dhGetShared testPriv testPub2
  ciphertext <- Key.encrypt key testMsg
  print (B.unpack key)
  print ciphertext
  pure $ Key.decrypt key ciphertext

testDHPoint :: ECDSA.PublicPoint
testDHPoint = DH.calculatePublic Key.sec_p256k1 (ECDSA.private_d testPriv)

testDHSecret :: ByteString
testDHSecret = B.convert $ DH.getShared Key.sec_p256k1 (ECDSA.private_d testPriv) testDHPoint

testDHSecret' :: Integer
testDHSecret' = CNS.os2ip $ DH.getShared Key.sec_p256k1 (ECDSA.private_d testPriv) testDHPoint

testKeyExport :: IO ()
testKeyExport = do
  (pub, priv) <- Key.new
  let pem = Key.exportPriv priv
  let Right (pub', priv') = Key.importPriv pem
  putStrLn pem
  print (pub==pub', priv==priv')

  let pem = Key.exportPub pub
  let Right pub'' = Key.importPub pem
  putStrLn pem
  print (pub==pub'')

-- Warning: If you change this, most of the tx serialization tests will fail;
-- The nonce is used to sign the transaction header, included in the tx body.
testNonce :: Integer
testNonce = 42

testSig :: ECDSA.Signature
testSig = ECDSA.Signature
  115136800820456833737994126771386015026287095034625623644186278108926690779567
  98245280522003505644797670843107276132602050133082625768706491602875725788467

-- | Test public key
testPub :: Key.PubKey
testPub = Key.PubKey
  ECDSA.PublicKey
    { public_curve = Key.sec_p256k1
    , public_q =
        ECC.Point
          79174541683660805620639640382768661759397823295690888507762753093299621916987
          71065057682299419085325030940046895916159750330092956837115984315857371796477
    }

-- | Test private key
testPriv :: ECDSA.PrivateKey
testPriv =
  ECDSA.PrivateKey
    { ECDSA.private_curve = Key.sec_p256k1
    , private_d =
        72637887363324669071595225655990695893413546682343152974463667925881860469868
    }

testPub2 :: Key.PubKey
testPub2 = Key.PubKey
  ECDSA.PublicKey
    { public_curve = Key.sec_p256k1,
      public_q =
        ECC.Point
          1214472788908201963423194854954899474972637934119143134374611198349064747631
          55018619338720117837520711191755301992479046934581364159114240410943276116468
    }

testPub3 :: Key.PubKey
testPub3= Key.PubKey
  ECDSA.PublicKey
    { public_curve = Key.sec_p256k1
    , public_q =
        ECC.Point
          33718916237022633221230485306632773657519572332333597686012127480807130421976
          58146723934287926414800677754909247104297877689488401591859022271503476599055
    }

testPriv2 :: ECDSA.PrivateKey
testPriv2 =
  ECDSA.PrivateKey
    { ECDSA.private_curve = Key.sec_p256k1
    , private_d =
        63397430372085899480856522915791132982175202462259856053849906393820683762766
    }

-------------------------------------------------------------------------------
-- Evaluation
-------------------------------------------------------------------------------

testTransactionCtx :: Eval.TransactionCtx
testTransactionCtx = Eval.TransactionCtx
  { transactionHash = toHash ("" :: Text)
  , transactionIssuer = testAddr3
  , transactionBlockTs = testTimestamp
  , transactionBlockIdx = 7
  }

