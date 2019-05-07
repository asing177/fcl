{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Reference where

import Protolude

import Control.Monad.Fail
import qualified Data.Binary      as B
import qualified Data.Map as Map
import qualified SafeString as SafeString
import           Encoding
import           Hash
import           Protolude
import qualified Time
import           Script.Parser
import Text.Parsec
import Script.Lexer
import Script.Pretty
import Asset
import qualified Storage
import qualified Fixed
import qualified Script.Eval as Eval
import           Text.Parsec.Text
import qualified Data.ByteString.Char8 as BS8
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import Crypto.Random.Types (MonadRandom(..))
import qualified Data.Serialize as S
import Contract
import Ledger
import qualified Key
import qualified Data.ByteString.Char8 as BSC

import           Test.Tasty.QuickCheck hiding (listOf)


type AS = Address AAsset
type AC = Address AAccount
type C = Address AContract

-------------------------------------------------------------------------------
-- Timestamp Fixtures
-------------------------------------------------------------------------------

testTimestamp :: Time.Timestamp
testTimestamp = 1231006505

---------------------------------------------
-- Addresses
---------------------------------------------

data AAccount = AAccount
  deriving (Show, Eq, Ord, Generic, Hash.Hashable)
data AAsset = AAsset
  deriving (Show, Eq, Ord, Generic, Hash.Hashable)

data AContract = AContract
  deriving (Show, Eq, Ord, Generic, Hash.Hashable)

newtype Address a = Address (Hash.Hash Encoding.Base58ByteString)
  deriving (Show, Read, Eq, Ord, Generic, B.Binary, Typeable, Hash.Hashable)

instance Arbitrary (Address a) where
  arbitrary = fromB58Hash <$> arbitrary

instance Pretty (Address a) where
  ppr (Address b) = squotes $ ppr $ Hash.getHash b

instance Pretty [Address a] where
  ppr as = listOf as

fromB58Hash :: Hash.Hash Encoding.Base58ByteString -> Address a
fromB58Hash = Address

genByteString :: Gen ByteString
genByteString =
  (toS :: [Char] -> ByteString) <$>
  (arbitrary `suchThat` (\s -> length s < SafeString.maxSize))

instance (Encoding.ByteStringEncoding a) => Arbitrary (Hash.Hash a) where
  arbitrary = Hash.toHash <$> genByteString

rawAddress :: Parser (Address a)
rawAddress
  = fromBS . BS8.pack <$> between (symbol "\'") (symbol "\'") (many1 alphaNum)

-- | Build an 'Address' from a raw base 58 encoded bytestring. ( Not safe )
fromBS :: ByteString -> Address a
fromBS bs =
    case Encoding.parseEncodedBS bs of
      Left err -> panicErr err
      Right (b58bs :: Encoding.Base58ByteString) ->
        case Hash.parseHash b58bs of
          Left err -> panicErr err
          Right h  -> Address h
  where
    panicErr err = panic $
      "Cannot validate address as input to 'fromRaw': " <> show err

addrAsParser :: Parser AS
addrAsParser = rawAddress

addrAcParser :: Parser AC
addrAcParser = rawAddress

addrCParser :: Parser C
addrCParser = rawAddress

type CustomParsers = Script.Parser.AddrParsers AS AC C
customAddrParsers :: CustomParsers
customAddrParsers = Script.Parser.AddrParsers
  { pAs = addrAsParser
  , pAc = addrAcParser
  , pC = addrCParser
  }

testAddr :: Address a
testAddr = deriveAddress testPub

testAddr2 :: Address a
testAddr2 = deriveAddress testPub2

testAddr3 :: Address a
testAddr3 = deriveAddress testPub3

deriveHash :: ByteString -> Hash.Hash Encoding.Base58ByteString
deriveHash = Hash.sha256 . Hash.sha256Raw . Hash.ripemd160Raw . Hash.sha256Raw

deriveAddress :: PublicKey -> Address a
deriveAddress (x, y) = Address (deriveHash pstr)
  where
    pstr   = show x <> show y

-------------------------------------------------------------------------------
-- Evaluation
-------------------------------------------------------------------------------

testTransactionCtx :: Eval.TransactionCtx (Address a)
testTransactionCtx = Eval.TransactionCtx
  { transactionHash = toHash ("" :: Text)
  , transactionIssuer = testAddr3
  , transactionBlockTs = testTimestamp
  , transactionBlockIdx = 7
  }

----------------------------------------
-- Keys
----------------------------------------

type PrivateKey = Integer
type PublicKey = (Integer, Integer)

instance Key.Key PrivateKey where
  sign = notImplemented
  
-- | Test private key
testPriv :: PrivateKey
testPriv = 72637887363324669071595225655990695893413546682343152974463667925881860469868

testPub :: PublicKey
testPub = (79174541683660805620639640382768661759397823295690888507762753093299621916987
          , 71065057682299419085325030940046895916159750330092956837115984315857371796477
          )

testPub2 :: PublicKey
testPub2 =
  ( 1214472788908201963423194854954899474972637934119143134374611198349064747631
  , 55018619338720117837520711191755301992479046934581364159114240410943276116468
  )

testPub3 :: PublicKey
testPub3 =
  ( 33718916237022633221230485306632773657519572332333597686012127480807130421976
  , 58146723934287926414800677754909247104297877689488401591859022271503476599055
  )

putSignature :: S.Putter ECDSA.Signature
putSignature (ECDSA.Signature r s) = do
    S.put $ safeEncInteger r
    S.put ':'
    S.put $ safeEncInteger s
  where
    safeEncInteger :: Integer -> SafeString.SafeString
    safeEncInteger = SafeString.fromBytes' . show

getSignature :: S.Get ECDSA.Signature
getSignature = do
  rSS <- S.get
  _ <- S.get :: S.Get Char
  sSS <- S.get
  let rBS = SafeString.toBytes rSS
  let sBS = SafeString.toBytes sSS
  let read' = head . reads . BSC.unpack
  let mRS = do
        r' <- read' rBS
        s' <- read' sBS
        return (fst r', fst s')
  case mRS of
    Nothing -> fail "Could not decode ECDSA.signature"
    Just (r,s) -> return $ ECDSA.Signature r s

----------------------------------
-- World
----------------------------------

data Account = Account
  { publicKey   :: PublicKey
  , address     :: AC
  , timezone    :: SafeString.SafeString
  , metadata    :: Metadata
  } deriving (Show, Eq, Generic, Hash.Hashable)

-- | A map of holdings to balances. i.e. a ledger
newtype Holdings = Holdings { unHoldings :: Map.Map (Holder AC C) Balance}
  deriving (Eq, Ord, Show, Generic)

data Asset = Asset
  { name      :: Text              -- ^ Name of asset
  , issuer    :: AC  -- ^ Issuer
  , issuedOn  :: Time.Timestamp         -- ^ Timestamp
  , supply    :: Balance           -- ^ Total supply
  , holdings  :: Holdings          -- ^ Holdings map
  , reference :: Maybe Ref         -- ^ Reference unit
  , assetType :: AssetType         -- ^ Asset type
  , address   :: AS    -- ^ Asset address
  , metadata  :: Metadata -- ^ Asset address
  } deriving (Eq, Show, Generic)


data World as ac c = World
  { contracts :: Map.Map c (Contract as ac c)
  , assets    :: Map.Map as Asset
  , accounts  :: Map.Map ac Account
  } deriving (Show, Eq, Generic)

instance (Semigroup as, Semigroup ac, Semigroup c, Ord as, Ord ac, Ord c) => Semigroup (World as ac c) where
  w1 <> w2 = World
    (contracts w1 <> contracts w2)
    (assets w1    <> assets w2)
    (accounts w1  <> accounts w2)

instance (Monoid as, Monoid ac, Monoid c, Ord as, Ord ac, Ord c) => Monoid (World as ac c) where
  mempty = World mempty mempty mempty

-- | Empty world state
genesisWorld :: World AS AC C
genesisWorld = World mempty mempty mempty

instance Ledger.Addressable Asset AS where
  toAddress = address

instance Ledger.Addressable Account AC where
  toAddress = address

instance Ledger.Addressable (Contract AS AC C) C where
  toAddress = address

validateTransferAddrs = notImplemented

instance forall as ac c. Ledger.WorldOps (World as ac c) where
  -- transferAsset :: (Num balance) => World as ac c -> as -> Holder ac c -> Holder ac c -> balance -> Either (AssetError as ac c) (World as ac c)
  transferAsset world assetAddr from to bal = do
    validateTransferAddrs world from to
    case lookupAsset assetAddr world of
      Left err -> Left $ AssetDoesNotExist assetAddr
      Right asset -> do
        asset' <- transferHoldings from to bal asset
        Right $ world { assets = Map.insert assetAddr asset' (assets world) }

  circulateAsset = notImplemented
  lookupAccount = notImplemented
  lookupAsset = notImplemented
  lookupContract = notImplemented
  calcBalance = notImplemented


-------------------------------------------------------------------------------
-- Transaction
-------------------------------------------------------------------------------

-- | Transaction
data Transaction = Transaction
  { header    :: TransactionHeader
  , signature :: Encoding.Base64PByteString
  , origin    :: AC
  } deriving (Show, Eq, Generic, Hash.Hashable)

-- | Transaction header
data TransactionHeader
  = TxContract TxContract  -- ^ Contract transaction
  | TxAsset TxAsset        -- ^ Asset transactions
  | TxAccount TxAccount    -- ^ Account transactions
  deriving (Show, Eq, Generic, Hash.Hashable)

data TxContract
  = CreateContract {
      contract :: SafeString.SafeString
  }
  | Call {
      address :: C
    , method  :: ByteString
    , args    :: [Storage.Value AS AC C]
  }
  deriving (Show, Eq, Generic, Hash.Hashable)

type Balance = Int64

newtype Metadata = Metadata
  { unMetadata :: Map.Map Text Text }
  deriving (Show, Eq, Generic, Hash.Hashable)

instance Semigroup Metadata where
  (Metadata m1) <> (Metadata m2) =
    Metadata $ m1 <> m2

instance Monoid Metadata where
  mempty = Metadata mempty

-- | An asset reference is metadata assigning a off-chain reference quantity to
-- a single unit of an on-chain asset.
data Ref
  = USD               -- ^ US Dollars
  | GBP               -- ^ British Pounds
  | EUR               -- ^ Euros
  | CHF               -- ^ Swiss Francs
  | Token             -- ^ Abstract token
  | Security          -- ^ Security
  deriving (Eq, Ord, Show, Read, Enum, Generic, Hash.Hashable)

-- | Type of an asset's value. Underlying value is always a Int64, but this
-- informs the representation and range of valid values.
data AssetType
  = Discrete               -- ^ Discrete (Non-zero integer value)
  | Fractional Fixed.PrecN -- ^ Fractional (Fixed point decimal value)
  | Binary                 -- ^ Binary (Held/Not-Held) (supply is +1 for held, 0 for not-held)
  deriving (Eq, Ord, Show, Read, Generic, Hash.Hashable)

data TxAsset
  = CreateAsset {
      assetName :: SafeString.SafeString          -- ^ Asset name
    , supply    :: Balance                          -- ^ Asset supply
    , reference :: Maybe Ref                -- ^ Asset reference
    , assetType :: AssetType                -- ^ Asset type
    , metadata  :: Metadata                       -- ^ Arbitrary additional metadata
  }

  | Transfer {
      assetAddr :: AS                 -- ^ Address of asset
    , toAddr    :: Holder AC C                         -- ^ Asset of Receiver
    , balance   :: Balance                        -- ^ Amount to transfer
  }

  | Circulate {
      assetAddr :: AS                 -- ^ Address of asset
    , amount    :: Balance                        -- ^ Amount to transfer
  }

  | RevokeAsset {
      address :: AS                  -- ^ Address of asset to revoke
  }
  deriving (Show, Eq, Generic, Hash.Hashable)

data TxAccount
  = CreateAccount {
      pubKey   :: SafeString.SafeString -- ^ Public key associated with the account
    , timezone :: SafeString.SafeString -- ^ Time zone
    , metadata :: Metadata              -- ^ Arbitrary additional metadata
  }
  | RevokeAccount {
      address   :: AC     -- ^ Issue a revocation of an account
  }
  deriving (Show, Eq, Generic, Hash.Hashable)

-- | Hash transaction
-- Transactions currently use base 16 encoding.
hashTransaction :: Transaction -> Hash.Hash Encoding.Base16ByteString
hashTransaction = Hash.toHash

-- | Computes a Ledger value address using a transaction hash:
-- base58Encode ( sha3_256 ( base16Encode ( sha3_256 ( binary ( TX ) ) ) ) )
transactionToAddress :: Transaction -> Address a
transactionToAddress =
  fromB58Hash . Hash.sha256 . Hash.getRawHash . hashTransaction

encodeSig =  notImplemented
sign = notImplemented
signSWith = notImplemented

testNonce :: Integer
testNonce = 42

-- | Create a new transaction.
newTransaction
  :: MonadRandom m
  => AC   -- ^ Origin Account Address
  -> PrivateKey     -- ^ Private Key
  -> TransactionHeader  -- ^ Transaction payload
  -> m Transaction
newTransaction origin privKey header = do
  sig    <- sign privKey header
  pure Transaction {
    header    = header
  , origin    = origin
  , signature = encodeSig sig
  }

toContractAddr :: MonadRandom m => ByteString -> m C
toContractAddr s = do
  let contractHdr = TxContract $ CreateContract (SafeString.fromBytes' s)
  transactionToAddress <$> newTransaction testAddr testPriv contractHdr

testCall :: TransactionHeader
testCall = TxContract Call {
    address = testAddr
  , method  = "get"
  , args    = []
  }

testTx :: TransactionHeader -> Transaction
testTx hdr = tx
  where
    Just sig = signSWith testPriv testNonce hdr
    tx = Transaction {
      header    = hdr
    , origin    = testAddr
    , signature = encodeSig sig
    }

-----------------------
-- Assets
-----------------------
-- | Transfer an amount of the asset supply to an account's holdings
-- circulateSupply :: Holder AC C -> Balance -> Asset -> Either (AssetError AS AC C) Asset
circulateSupply addr bal asset
  | supply asset >= bal
    = Right $ asset { holdings = holdings', supply = supply' }
  | otherwise
    = Left $ InsufficientSupply (address asset) (supply asset)
  where
    holdings' = Holdings $ clearZeroes $
      Map.insertWith (+) addr bal $ unHoldings (holdings asset)
    supply' = supply asset - bal

    clearZeroes = Map.filter (/= 0)

-- | Atomically transfer holdings 'from' one account 'to' another
transferHoldings
  :: (Num balance)
  => Holder ac c
  -> Holder ac c
  -> balance
  -> Asset
  -> Either (AssetError as ac c) Asset
transferHoldings from to amount asset
  | from == to = Left $ SelfTransfer from
  | otherwise  =
      case balance asset from of
        Nothing ->
          Left $ HolderDoesNotExist from
        Just bal
          | amount <= bal -> do
              asset' <- circulateSupply from (negate amount) asset
              circulateSupply to amount asset'
          | otherwise     ->
              Left $ InsufficientHoldings from bal
  where
    assetIssuer = issuer asset
