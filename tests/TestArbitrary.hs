{-# LANGUAGE DataKinds #-}

module TestArbitrary where

import Protolude

import qualified Language.FCL.SafeInteger as SI
import qualified Language.FCL.SafeString as SS

import Test.Tasty.QuickCheck

import qualified Data.Map as Map
import qualified Data.ByteString as BS
import Language.FCL.Address as Address
import Language.FCL.Metadata as Metadata
import qualified Language.FCL.Encoding as Encoding
import qualified Language.FCL.Hash as Hash
import Language.FCL.AST
import qualified Language.FCL.Asset as Asset

import qualified Datetime.Types as DT
import qualified Data.Hourglass as DH
import qualified Data.Set as Set (fromList)
import qualified Data.Time.Calendar as DC
import qualified Data.Text as T
import Reference

import TestNumber()

-- TODO: Should we avoid orphan instances?

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
  arbitrary = SS.fromBytes' . toS <$> (arbitrary :: Gen Text)

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
      , pure VVoid
      , VDateTime <$> arbitrary
      , VTimeDelta <$> arbitrary
      , VEnum <$> arbitrary
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

instance Arbitrary EnumConstr where
  arbitrary = EnumConstr <$> arbitrary

instance Arbitrary Name where
  arbitrary = Name <$> arbitrary

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

instance Arbitrary T.Text where
  arbitrary = do -- generate non-empty printable ASCII strings
    x <- choose ('\65', '\90')
    xs <- listOf (choose ('\65', '\90'))
    pure (toS (x:xs))

instance Arbitrary Reference.Ref where
  arbitrary = elements [ minBound.. maxBound ]

instance Arbitrary Asset.AssetType where
  arbitrary = oneof
    [ pure Asset.Discrete
    -- , Asset.Fractional <$> arbitrary
    , pure Asset.Binary
    ]

instance Arbitrary Metadata where
  arbitrary = Metadata <$> arbitrary

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
