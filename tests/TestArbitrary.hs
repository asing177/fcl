{-# LANGUAGE DataKinds #-}

module TestArbitrary where

import Protolude

import qualified Data.Map as Map
import qualified Data.ByteString as BS
import Data.String (fromString)
import qualified Datetime.Types as DT
import qualified Data.Hourglass as DH
import qualified Data.Set as Set (fromList)
import qualified Data.Time.Calendar as DC
import qualified Data.Text as T
import Test.Tasty.QuickCheck

import qualified Language.FCL.SafeInteger as SI
import qualified Language.FCL.SafeString as SS
import Language.FCL.Token (keywords)
import Language.FCL.Address as Address
import Language.FCL.Metadata as Metadata
import qualified Language.FCL.Encoding as Encoding
import qualified Language.FCL.Hash as Hash
import Language.FCL.AST
import qualified Language.FCL.Asset as Asset

import Reference
import TestNumber()

-- TODO: Should we avoid orphan instances?

nonEmptyOf :: Gen a -> Gen (NonEmpty a)
nonEmptyOf gen = (:|) <$> gen <*> listOf gen

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = nonEmptyOf arbitrary

instance Arbitrary SI.SafeInteger where
  arbitrary =
    let minBound' = SI.fromSafeInteger minBound
        maxBound' = SI.fromSafeInteger maxBound
    in SI.toSafeInteger' <$> choose (minBound',maxBound')

newtype UnsafeInteger = UnsafeInteger Integer
  deriving Show

instance Arbitrary UnsafeInteger where
  arbitrary = UnsafeInteger <$> choose (n*2, n*(2^4096))
    where
      n = SI.fromSafeInteger maxBound

genByteString :: Gen BS.ByteString
genByteString =
  (toS :: [Char] -> BS.ByteString) <$>
  (arbitrary `suchThat` (\s -> length s < SS.maxSize))

instance Arbitrary SS.SafeString where
  arbitrary = SS.fromBytes' . toS <$> listOf alphaNum

instance Arbitrary (Address a) where
  arbitrary = Address . Hash.getRawHash . (Hash.toHash :: ByteString -> Hash.Hash Encoding.Base58ByteString) <$> genByteString

instance (Encoding.ByteStringEncoding a) => Arbitrary (Hash.Hash a) where
  arbitrary = Hash.toHash <$> genByteString

instance Arbitrary DT.Datetime where
  arbitrary = DT.posixToDatetime <$> choose (1, 32503680000) -- (01/01/1970, 01/01/3000)

instance Arbitrary DT.Period where
  arbitrary = do
    year <- choose (0,1000)
    month <- choose (0,12)
    let monthNumDays = DC.gregorianMonthLength (fromIntegral year) (fromIntegral month)
    day <- choose (0, monthNumDays)
    pure $ DT.Period $ DH.Period year month day

instance Arbitrary DT.Duration where
  arbitrary = fmap DT.Duration $ DH.Duration
    <$> (fmap DH.Hours $ choose (0,23))
    <*> (fmap DH.Minutes $ choose (0,59))
    <*> (fmap DH.Seconds $ choose (0,59))
    <*> pure 0

instance Arbitrary DT.Delta where
  arbitrary = DT.Delta <$> arbitrary <*> arbitrary

arbValue :: Int -> Gen Value
arbValue n
  | n <= 0
    = oneof
      [ VNum <$> arbitrary
      , VBool <$> arbitrary
      , VAccount <$> arbitrary
      , VAsset <$> arbitrary
      , VText <$> arbitrary
      , VSig <$> arbitrary
      , VDateTime <$> arbitrary
      , VTimeDelta <$> arbitrary
      , VConstr <$> arbitrary <*> arbitrary
      , VState <$> arbitrary
      , pure VUndefined
      ]
  | otherwise
    = oneof
      [ VMap . Map.fromList <$> listOf (liftArbitrary2 (arbValue (n - 1)) (arbValue (n - 1)))
      , VSet . Set.fromList <$> listOf (arbValue (n - 1))
      ]

instance Arbitrary Value where
  arbitrary = oneof [arbValue 0, arbValue 1]

instance Arbitrary DateTime where
  arbitrary = DateTime <$> arbitrary

instance Arbitrary TimeDelta where
  arbitrary = TimeDelta <$> arbitrary

instance Arbitrary Name where
  arbitrary = fromString
    <$> ((:) <$> elements ['a'..'z'] <*> listOf alphaNum)
      `suchThat` (not . (`elem` keywords) . toS)

instance Arbitrary NameUpper where
  arbitrary = fromString <$> ((:) <$> elements ['A'..'Z'] <*> listOf alphaNum)

alphaNum :: Gen Char
alphaNum = elements $ ['A'..'Z'] <> ['a'..'z'] <> ['0'..'9']

instance Arbitrary Place where
  arbitrary = Place <$> arbitrary

instance Arbitrary WorkflowState where
  arbitrary = oneof
    [ unsafeWorkflowState <$> do
        hd <- arbitrary
        tl <- arbitrary
        pure $ Set.fromList (hd:tl)
    , pure startState
    , pure endState
    ]

instance Arbitrary Reference.Ref where
  arbitrary = elements [ minBound.. maxBound ]

instance Arbitrary Asset.AssetType where
  arbitrary = oneof
    [ pure Asset.Discrete
    -- , Asset.Fractional <$> arbitrary
    , pure Asset.Binary
    ]

instance Arbitrary Metadata where
  arbitrary = Metadata . Map.fromList <$> listOf arbitraryPairs
    where
      arbitraryPairs = (,) <$> arbitraryText <*> arbitraryText
      arbitraryText = fromString <$> listOf alphaNum

instance Arbitrary Asset.Holder where
  arbitrary = oneof
    [ Asset.AccountHolder <$> (arbitrary :: Gen (Address AAccount))
    -- Warning: See Serialize Holder on Asset.hs
    -- Holder serializer defaults to Holder (Address AAccount)
    -- TODO: Fix it in the future and uncomment the following line
    -- , Asset.Holder <$> (arbitrary :: Gen (Address AContract))
    ]

-- instance Arbitrary TxAsset where
--   arbitrary = oneof
--     [ CreateAsset <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
--     , Transfer <$> arbitrary <*> arbitrary <*> arbitrary
--     , Circulate <$> arbitrary <*> arbitrary
--     , RevokeAsset <$> arbitrary
--     ]

-- instance Arbitrary TxAccount where
--   arbitrary = oneof
--     [ CreateAccount <$> arbitrary <*> arbitrary <*> arbitrary
--     , RevokeAccount <$> arbitrary
--     ]

-- instance Arbitrary TransactionHeader where
--   arbitrary = oneof
--     [ TxContract <$> arbitrary
--     , TxAsset <$> arbitrary
--     , TxAccount <$> arbitrary
--     ]
