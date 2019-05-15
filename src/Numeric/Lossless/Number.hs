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

import Protolude hiding (Hashable, option, show, lift)

import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Serialize (Serialize(..))

import Numeric.Lossless.Decimal
import Script.Pretty (Pretty(..))
import Hash (Hashable(..))

data Number = NumDecimal Decimal | NumRational Rational
  deriving (Show, Generic, Hash.Hashable, Serialize, FromJSON, ToJSON)

instance Num Number where
  NumDecimal  f1 + NumDecimal  f2 = NumDecimal  (f1           + f2)
  NumDecimal  d  + NumRational r  = NumRational (toRational d + r)
  NumRational r  + NumDecimal  d  = NumRational (r            + toRational d)
  NumRational r1 + NumRational r2 = NumRational (r1           + r2)

  NumDecimal  f1 - NumDecimal  f2 = NumDecimal  (f1           - f2)
  NumDecimal  d  - NumRational r  = NumRational (toRational d - r)
  NumRational r  - NumDecimal  d  = NumRational (r            - toRational d)
  NumRational r1 - NumRational r2 = NumRational (r1           - r2)

  NumDecimal  f1 * NumDecimal  f2 = NumDecimal  (f1           * f2)
  NumDecimal  d  * NumRational r  = NumRational (toRational d * r)
  NumRational r  * NumDecimal  d  = NumRational (r            * toRational d)
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
  toRational (NumRational r) = r

instance Fractional Number where
  fromRational = NumRational

  n1 / n2 = NumRational (toRational n1 / toRational n2)

instance Pretty Number where
  ppr (NumDecimal n)  = ppr n
  ppr (NumRational n) = ppr n


