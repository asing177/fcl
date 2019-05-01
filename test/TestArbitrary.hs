module TestArbitrary where

import           Protolude

import qualified SafeInteger           as SI
import qualified SafeString            as SS

import qualified Data.Hourglass        as DH
import qualified Data.Hourglass.Types  as DH
import qualified Data.Map              as Map
import qualified Data.Set              as Set (fromList)
import qualified Datetime              as DT
import qualified Datetime.Types        as DT
import qualified Hash

import qualified Data.Time.Calendar    as DC

import qualified Encoding
import qualified Fixed
import qualified Script

import           Test.Tasty.QuickCheck

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

genByteString :: Gen ByteString
genByteString =
  (toS :: [Char] -> ByteString) <$>
  (arbitrary `suchThat` (\s -> length s < SS.maxSize))

instance Arbitrary SS.SafeString where
  arbitrary = SS.fromBytes' . toS <$> (arbitrary :: Gen Text)

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

instance Arbitrary Fixed.FixedN where
  arbitrary = Fixed.mkFixed <$> arbitrary <*> arbitrary

instance Arbitrary Fixed.PrecN where
  arbitrary = elements
    [ Fixed.Prec1
    , Fixed.Prec2
    , Fixed.Prec3
    , Fixed.Prec4
    , Fixed.Prec5
    , Fixed.Prec6
    ]

arbValue :: (Ord as, Ord ac, Ord c, Arbitrary as, Arbitrary ac, Arbitrary c) => Int -> Gen (Script.Value as ac c)
arbValue n
  | n <= 0
    = oneof
      [ Script.VInt <$> arbitrary
      , Script.VFloat <$> arbitrary
      , Script.VFixed <$> arbitrary
      , Script.VBool <$> arbitrary
      , Script.VAccount <$> arbitrary
      , Script.VAsset <$> arbitrary
      , Script.VText <$> arbitrary
      , Script.VSig <$> arbitrary
      , pure Script.VVoid
      , Script.VDateTime <$> arbitrary
      , Script.VTimeDelta <$> arbitrary
      , Script.VEnum <$> arbitrary
      , Script.VState <$> arbitrary
      , pure Script.VUndefined
      ]
  | otherwise
    = oneof
      [ Script.VMap . Map.fromList <$> listOf (liftArbitrary2 (arbValue (n - 1)) (arbValue (n - 1)))
      , Script.VSet . Set.fromList <$> listOf (arbValue (n - 1))
      ]

instance (Ord as, Ord ac, Ord c, Arbitrary as, Arbitrary ac, Arbitrary c) => Arbitrary (Script.Value as ac c) where
  arbitrary = oneof [arbValue 0, arbValue 1]

instance Arbitrary Script.DateTime where
  arbitrary = Script.DateTime <$> arbitrary

instance Arbitrary Script.TimeDelta where
  arbitrary = Script.TimeDelta <$> arbitrary

instance Arbitrary Script.EnumConstr where
  arbitrary = Script.EnumConstr <$> arbitrary

instance Arbitrary Script.Name where
  arbitrary = Script.Name <$> arbitrary

instance Arbitrary Script.Place where
  arbitrary = Script.Place <$> arbitrary

instance Arbitrary Script.WorkflowState where
  arbitrary = oneof
    [ Script.unsafeWorkflowState <$> do
        hd <- arbitrary
        tl <- arbitrary
        pure $ Set.fromList (hd:tl)
    , pure Script.startState
    , pure Script.endState
    ]

instance Arbitrary Text where
  arbitrary = do -- generate non-empty printable ASCII strings
    x <- choose ('\65', '\90')
    xs <- listOf (choose ('\65', '\90'))
    pure (toS (x:xs))
