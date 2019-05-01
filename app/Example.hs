{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Example where

import qualified Data.Binary      as B
import           Encoding
import           Hash
import           Protolude
import           Script.Parser
import Text.Parsec
import Script.Lexer
import Script.Pretty
import           Text.Parsec.Text
import qualified Data.ByteString.Char8 as BS8

data AAccount = AAccount
data AAsset = AAsset
data AContract = AContract

newtype Address a = Address (Hash.Hash Encoding.Base58ByteString)
  deriving (Show, Read, Eq, Ord, Generic, NFData, B.Binary, Typeable, Hash.Hashable)

instance Pretty (Address a) where
  ppr (Address b) = squotes $ ppr $ Hash.getHash b

instance Pretty [Address a] where
  ppr as = listOf as

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

customAddrParsers :: Script.Parser.AddrParsers (Address AAsset) (Address AAccount) (Address AContract)
customAddrParsers = Script.Parser.AddrParsers
  { pAs = addrAsParser
  , pAc = addrAcParser
  , pC = addrCParser
  }

