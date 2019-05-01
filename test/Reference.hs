{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Reference where

import Protolude

import qualified Data.Binary      as B
import qualified SafeString as SS
import           Encoding
import           Hash
import           Protolude
import           Script.Parser
import Text.Parsec
import Script.Lexer
import Script.Pretty
import           Text.Parsec.Text
import qualified Data.ByteString.Char8 as BS8
import           Test.Tasty.QuickCheck hiding (listOf)

---------------------------------------------
-- Addresses
---------------------------------------------

data AAccount = AAccount
  deriving (Show, Eq, Ord)
data AAsset = AAsset
  deriving (Show, Eq, Ord)

data AContract = AContract
  deriving (Show, Eq, Ord)

newtype Address a = Address (Hash.Hash Encoding.Base58ByteString)
  deriving (Show, Read, Eq, Ord, Generic, NFData, B.Binary, Typeable, Hash.Hashable)

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
  (arbitrary `suchThat` (\s -> length s < SS.maxSize))
  
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

addrAsParser :: Parser (Address AAsset)
addrAsParser = rawAddress

addrAcParser :: Parser (Address AAccount)
addrAcParser = rawAddress

addrCParser :: Parser (Address AContract)
addrCParser = rawAddress

type CustomParsers = Script.Parser.AddrParsers (Address AAsset) (Address AAccount) (Address AContract)
customAddrParsers :: CustomParsers
customAddrParsers = Script.Parser.AddrParsers
  { pAs = addrAsParser
  , pAc = addrAcParser
  , pC = addrCParser
  }

testAddr :: Address a
testAddr = deriveAddress testPub

deriveHash :: ByteString -> Hash.Hash Encoding.Base58ByteString
deriveHash = Hash.sha256 . Hash.sha256Raw . Hash.ripemd160Raw . Hash.sha256Raw

deriveAddress :: PublicKey -> Address a
deriveAddress (x, y) = Address (deriveHash pstr)
  where
    pstr   = show x <> show y

----------------------------------------
-- Keys
----------------------------------------

type PrivateKey = Integer
type PublicKey = (Integer, Integer)

-- | Test private key
testPriv :: PrivateKey
testPriv = 72637887363324669071595225655990695893413546682343152974463667925881860469868

testPub :: PublicKey
testPub = (79174541683660805620639640382768661759397823295690888507762753093299621916987
          , 71065057682299419085325030940046895916159750330092956837115984315857371796477
          )
