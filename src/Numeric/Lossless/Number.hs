{-|
Module      : Numeric.Lossless.Number
Description : Rational numbers with a possibly known finite decimal precision.

Rational numbers are useful because they let us do proper field arithmetic.
However in financial applications, we mostly want to work with a fixed number of
decimal places. When dividing, we don't know how many decimal places we will
need to write down the result accurately--potentially an infinite number of
decimal places.

Use the rounding operations from @Numeric.Lossless.Decimal@ to explicitly
truncate to a certain precision.
-}

{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

module Numeric.Lossless.Number
  ( Number(..),
    module Numeric.Lossless.Decimal -- rexport
  )
  where

import Data.Aeson as A
import Data.Serialize (Serialize(..))
import Fraction (Fraction(..))
import Protolude hiding (Hashable, option, show, lift)
import Test.QuickCheck

import Numeric.Lossless.Decimal
import Language.FCL.Pretty (Pretty(..))
import Language.FCL.Hash (Hashable(..))
import Language.FCL.Orphans ()

data Number = NumDecimal Decimal | NumRational Fraction
  deriving (Show, Generic, Hashable, Serialize)

instance ToJSON Number where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON Number where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance Num Number where
  NumDecimal  f1 + NumDecimal  f2 = NumDecimal  (f1           + f2)
  NumDecimal  d  + NumRational r  = NumRational (realToFrac d + r)
  NumRational r  + NumDecimal  d  = NumRational (r            + realToFrac d)
  NumRational r1 + NumRational r2 = NumRational (r1           + r2)

  NumDecimal  f1 - NumDecimal  f2 = NumDecimal  (f1           - f2)
  NumDecimal  d  - NumRational r  = NumRational (realToFrac d - r)
  NumRational r  - NumDecimal  d  = NumRational (r            - realToFrac d)
  NumRational r1 - NumRational r2 = NumRational (r1           - r2)

  NumDecimal  f1 * NumDecimal  f2 = NumDecimal  (f1           * f2)
  NumDecimal  d  * NumRational r  = NumRational (realToFrac d * r)
  NumRational r  * NumDecimal  d  = NumRational (r            * realToFrac d)
  NumRational r1 * NumRational r2 = NumRational (r1           * r2)

  abs (NumDecimal d)  = NumDecimal (abs d)
  abs (NumRational r) = NumRational (abs r)

  signum (NumDecimal d)  = NumDecimal (signum d)
  signum (NumRational r) = NumRational (signum r)

  fromInteger = NumDecimal . fromInteger

instance Eq Number where
  n1 == n2 = (toRational n1) == (toRational n2)

instance Ord Number where
  compare n1 n2 = compare (toRational n1) (toRational n2)

instance Real Number where
  toRational (NumDecimal d)  = toRational d
  toRational (NumRational r) = toRational r

instance Fractional Number where
  fromRational = realToFrac

  NumRational n1 / NumRational n2 = NumRational (n1 / n2)
  NumDecimal  n1 / NumDecimal  n2 = NumRational (realToFrac n1 / realToFrac n2)
  NumRational n1 / NumDecimal  n2 = NumRational (n1 / realToFrac n2)
  NumDecimal  n1 / NumRational n2 = NumRational (realToFrac n1 / n2)

instance Pretty Number where
  ppr (NumDecimal n)  = ppr n
  ppr (NumRational n) = ppr n


---------------
-- Arbitrary --
---------------

instance Arbitrary Number where
  arbitrary = oneof
      [ NumDecimal <$> arbitrary
      , NumRational <$> arbitrary
      ]
