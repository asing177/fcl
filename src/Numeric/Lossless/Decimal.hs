{-|
Module      : Numeric.Lossless.Decimal
Description : Lossless arbitrary precision decimal arithmetic.

Lossless decimal numbers e.g. for financial calculations. As opposed to floating
or fixed point numbers, arithemitc with 'Decimal's is guaranteed to preserve all
information. We increase the precision pessimistically as required by the
supported arithmetic operations '(+)', '(-)', '(*)'.

Let @P@ be a mapping from 'Decimal's to their precision, then for all 'Decimal's
@m@ and @n@:

  @
    P(m+n) = max(P(m), P(n))  -- (similarly for subtraction)
    P(m*n) = P(m) + P(n)
  @

The encoding of a finite rational @n@ consists of pairs of 'Integer's @p@,
denoting the precision (number of decimal places), and @k@, denoting the integer
such that @k*10^-p = n@.

'Decimal's do not support a division operation, since in the general case this
requires an infinite  precision, e.g. @1/3 = 0.333...@. If you need to divide,
use 'toRational' and then convert the result back to 'Decimal's via one of the
rounding operations provided in this module.

-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Numeric.Lossless.Decimal (
  Decimal(..),
  roundUp,
  roundUpRem,
  roundDown,
  roundDownRem,
  roundAwayFrom0,
  roundAwayFrom0Rem,
  rationalToDecimalAndRemainder,
  fracToDec,
  decToFrac
) where

import Protolude hiding (Hashable, option, show, lift)
import Test.QuickCheck

import Data.Aeson as A
import Data.Binary (Binary(..))
import Data.Char (isDigit)
import Data.List (genericLength, genericReplicate, genericSplitAt)
import Data.Serialize (Serialize(..))
import Fraction (Fraction(..))
import Text.ParserCombinators.ReadP
import Text.Read
import Text.Show

import Language.FCL.Pretty (Pretty(..))
import Language.FCL.Hash (Hashable(..))
import Language.FCL.Utils ((?), putBinaryViaSerialize, getBinaryViaSerialize)

data Decimal = Decimal
  { decimalPlaces :: Integer
  , decimalIntegerValue :: Integer
  } deriving (Generic, Hashable, Serialize)

instance ToJSON Decimal where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON Decimal where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

--------------------------------------------------------------------------------
-- * Maths-related instances
--------------------------------------------------------------------------------

instance Eq Decimal where
  x == y = toRational x == toRational y

instance Ord Decimal where
  compare x y = compare (toRational x) (toRational y)

instance Num Decimal where
  Decimal p1 v1 + Decimal p2 v2
    | p1 > p2   = Decimal p1 (v1 + (v2 * 10^(p1 - p2)))
    | otherwise = Decimal p2 ((v1 * 10^(p2 - p1)) + v2)

  Decimal p1 v1 - Decimal p2 v2
    | p1 > p2   = Decimal p1 (v1 - (v2 * 10^(p1 - p2)))
    | otherwise = Decimal p2 ((v1 * 10^(p2 - p1)) - v2)

  Decimal p1 v1 * Decimal p2 v2 = Decimal (p1 + p2) (v1 * v2)

  abs (Decimal p v) = Decimal p (abs v)

  signum = fromInteger . signum . decimalIntegerValue

  fromInteger = Decimal 0

instance Real Decimal where
  toRational (Decimal p v)
    | p < 0     = v * 10^(-p) % 1 -- (^) errors on negative exponent
    | otherwise = v % 10^p

--------------------------------------------------------------------------------
-- * 'Rational' to 'Decimal' conversions
--------------------------------------------------------------------------------

-- | Given a precision @p :: Integer@ and an @n :: Rational@, find the pair
-- @(d,r) :: (Decimal, Rational)@ where @d@ is the greatest 'Decimal' with
-- precision @p@ and @r@ is the least 'Rational', such that @d + r = n@
rationalToDecimalAndRemainder :: Integer -> Rational -> (Decimal, Rational)
rationalToDecimalAndRemainder p n = (Decimal p i, remainder)
  where
    (i, remainder) -- case split because (^) errors on negative exponent
      | p < 0     = second (* 10^(-p)) (properFraction (n / 10^(-p)))
      | otherwise = second (/ 10^p)    (properFraction (10^p * n))

-- | Convert Fraction to Decimal
fracToDec :: Integer -> Fraction -> Decimal
fracToDec p = fst . rationalToDecimalAndRemainder p . toRational

-- | Convert Decimal to Fraction
decToFrac :: Decimal -> Fraction
decToFrac Decimal{..} = decimalIntegerValue :-: (10^decimalPlaces)

roundDown :: Integer -> Rational -> Decimal
roundDown i n = fst $ rationalToDecimalAndRemainder i n

roundDownRem :: Integer -> Rational -> Rational
roundDownRem i n = snd $ rationalToDecimalAndRemainder i n

roundUp :: Integer -> Rational -> Decimal
roundUp i n = case rationalToDecimalAndRemainder i n of
  (d, 0) -> d
  (Decimal p i, _) -> Decimal p (i + 1)

roundUpRem :: Integer -> Rational -> Rational
roundUpRem i n = n - toRational (roundUp i n)

roundAwayFrom0 :: Integer -> Rational -> Decimal
roundAwayFrom0 i n = case rationalToDecimalAndRemainder i n of
  (Decimal p i, remainder) | remainder >= (1 % 2)    -> Decimal p (i + 1)
  (Decimal p i, remainder) | remainder <= ((-1) % 2) -> Decimal p (i - 1)
  (d, _) -> d

roundAwayFrom0Rem :: Integer -> Rational -> Rational
roundAwayFrom0Rem i n = n - toRational (roundUp i n)

--------------------------------------------------------------------------------
-- * More instances
--------------------------------------------------------------------------------

instance Pretty Decimal where
  ppr = ppr . show

instance Show Decimal where
  show (Decimal decimalPlaces value)
      | decimalPlaces < 0
        = show value <> "e" <> show (abs decimalPlaces)
      | decimalPlaces == 0
        = show value
      | otherwise
        = ((value < 0 ? "-") <>)
        . reverse
        . decimalPl
        . genericSplitAt decimalPlaces
        . reverse
        . show
        . abs
        $ value
    where
      decimalPl (decimalPart,[])
        = decimalPart
        <> genericReplicate (decimalPlaces - genericLength decimalPart) '0'
        <> ".0"
      decimalPl (decimalPart, integerPart)
        = decimalPart
        <> "."
        <> integerPart

instance Read Decimal where
  readPrec = lift $ do
      sign <- parseSign
      int <- many1 $ satisfy isDigit
      dec <- option "" $ do
        char '.'
        many1 (satisfy isDigit)
      exp <- option 0 $ do
        char 'e'
        sign <- parseSign
        n <- many1 $ satisfy isDigit
        pure (read (sign <> n)) -- can't fail
      pure $ Decimal
        (genericLength dec - exp)
        (read $ sign <> int <> dec)
    where
      parseSign = option "" (string "-")

instance Binary Decimal where
  put = putBinaryViaSerialize
  get = getBinaryViaSerialize


---------------
-- Arbitrary --
---------------

instance Arbitrary Decimal where
  arbitrary = Decimal
    <$> arbitrary
    <*> liftA2 (^) arbitrary (arbitrary :: Gen Word8)
