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
import Fraction (Fraction(..), Transcendental(..))
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

instance Transcendental Number where
  pi' eps = NumRational $ pi' eps

  tan' eps (NumDecimal n) = NumDecimal $ convDec tan' eps n
  tan' eps (NumRational n) = NumRational $ tan' eps n

  sin' eps (NumDecimal n) = NumDecimal $ convDec sin' eps n
  sin' eps (NumRational n) = NumRational $ sin' eps n

  cos' eps (NumDecimal n) = NumDecimal $ convDec cos' eps n
  cos' eps (NumRational n) = NumRational $ cos' eps n

  atan' eps (NumDecimal n) = NumDecimal $ convDec atan' eps n
  atan' eps (NumRational n) = NumRational $ atan' eps n

  asin' eps (NumDecimal n) = NumDecimal $ convDec asin' eps n
  asin' eps (NumRational n) = NumRational $ asin' eps n

  acos' eps (NumDecimal n) = NumDecimal $ convDec cos' eps n
  acos' eps (NumRational n) = NumRational $ acos' eps n

  sqrt' eps (NumDecimal n) = NumDecimal $ convDec sqrt' eps n
  sqrt' eps (NumRational n) = NumRational $ sqrt' eps n

  root' eps (NumDecimal n@(Decimal p v)) k = NumDecimal $ fracToDec (epsToPrec eps) (root' eps (decToFrac n) k)
  root' eps (NumRational n) k = NumRational $ root' eps n k

  power' eps (NumDecimal n) (NumDecimal m)
    = NumDecimal $ fracToDec (epsToPrec eps) (power' eps (decToFrac n) (decToFrac m))
  power' eps (NumRational n) (NumDecimal m)
    = NumRational $ power' eps n (decToFrac m)
  power' eps (NumDecimal n) (NumRational m)
    = NumRational $ power' eps (decToFrac n) m
  power' eps (NumRational n) (NumRational m)
    = NumRational $ power' eps n m

  exp' eps (NumDecimal n) = NumDecimal $ convDec exp' eps n
  exp' eps (NumRational n) = NumRational $ exp' eps n

  tanh' eps (NumDecimal n) = NumDecimal $ convDec tanh' eps n
  tanh' eps (NumRational n) = NumRational $ tanh' eps n

  sinh' eps (NumDecimal n) = NumDecimal $ convDec sinh' eps n
  sinh' eps (NumRational n) = NumRational $ sinh' eps n

  cosh' eps (NumDecimal n) = NumDecimal $ convDec cosh' eps n
  cosh' eps (NumRational n) = NumRational $ cosh' eps n

  atanh' eps (NumDecimal n) = NumDecimal $ convDec atanh' eps n
  atanh' eps (NumRational n) = NumRational $ atanh' eps n

  asinh' eps (NumDecimal n) = NumDecimal $ convDec asinh' eps n
  asinh' eps (NumRational n) = NumRational $ asinh' eps n

  acosh' eps (NumDecimal n) = NumDecimal $ convDec acosh' eps n
  acosh' eps (NumRational n) = NumRational $ acosh' eps n

  log' eps (NumDecimal n) = NumDecimal $ convDec log' eps n
  log' eps (NumRational n) = NumRational $ log' eps n

  decimal eps (NumDecimal n) = decimal eps (decToFrac n)
  decimal eps (NumRational n) = decimal eps n

-- Helper functions for Transcendental instance of Number --

convDec :: (Fraction -> Fraction -> Fraction) -> Fraction -> Decimal -> Decimal
convDec f eps (n@(Decimal p v)) = fracToDec (epsToPrec eps) (f eps (decToFrac n))

epsToPrec :: Fraction -> Integer
epsToPrec eps = decimalIntegerValue . fracToDec 0 $ recip eps

---------------
-- Arbitrary --
---------------

instance Arbitrary Number where
  arbitrary = oneof
      [ NumDecimal <$> arbitrary
      , NumRational <$> arbitrary
      ]
