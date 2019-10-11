{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Number where

import Protolude
import Control.Applicative (liftA2)
import qualified Data.Binary as Binary (decode, encode)
import Data.Word (Word8)
import Text.Read (readMaybe)
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
import Text.Read (read)

import Language.FCL.Parser (parseDecimal)
import Language.FCL.Pretty (prettyPrint)
import Numeric.Lossless.Number

-- Type aliases to allow running the tests on other types, e.g. Double
type DecimalT = Decimal
type NumberT = Number

numberTests :: TestTree
numberTests = testGroup "Arithmetic properties"

  [ testProperty "Decimal addition is not lossy" $
      \(n1 :: DecimalT) (n2 :: DecimalT) ->
        toRational (n1 + n2) == toRational n1 + toRational n2

  , testProperty "Decimal subtraction is not lossy" $
      \(n1 :: DecimalT) (n2 :: DecimalT) ->
        toRational (n1 - n2) == toRational n1 - toRational n2

  , testProperty "Decimal multiplication is not lossy" $
      \(n1 :: DecimalT) (n2 :: DecimalT) ->
        toRational (n1 * n2) == toRational n1 * toRational n2

  , testProperty "Division is not lossy" $
      \(n1 :: NumberT) (n2 :: NumberT) ->
        n2 /= 0 ==>
          toRational (n1 / n2) == toRational n1 / toRational n2

  , testProperty "Addition is associative" $
      \(a :: NumberT) (b :: NumberT) (c :: NumberT) ->
        (a + b) + c == a + (b + c)

  , testProperty "Addition is commutative" $
        \(a :: NumberT) (b :: NumberT) ->
          a + b == b + a

  , testProperty "Multiplication is associative" $
      \(a :: NumberT) (b :: NumberT) (c :: NumberT) ->
        (a * b) * c == a * (b * c)

  , testProperty "Multiplicative inverse" $
      \(n :: NumberT) ->
          n /= 0 ==>
            n * (1/n) == 1

  , testProperty "Division distributes over addition" $
      \(a :: NumberT) (b :: NumberT) (c :: NumberT) ->
        c /= 0 ==>
          (a + b) / c == (a / c) + (b / c)

  , testProperty "Rounding and remainder add up to same number" $
      \(p :: Integer) (r :: Rational) ->
        let (decimal, remainder) = rationalToDecimalAndRemainder p r
        in toRational decimal + remainder == r

  , testProperty "rationalToDecimal . toRational == id" $
      \(d :: Decimal) ->
        rationalToDecimalAndRemainder (decimalPlaces d) (toRational d) == (d, 0)

  , testProperty "Roundtrip Decimal serialisation" $
      \(d :: Decimal) ->
        Binary.decode (Binary.encode d) == d

  , testProperty "Roundtrip read and show" $
      \(d :: Decimal) ->
        readMaybe (show d) == Just d

  , testProperty "Roundtrip parsing and pretty-printing" $
      \(d :: Decimal) ->
        parseDecimal (prettyPrint d) == Right d
  ]
