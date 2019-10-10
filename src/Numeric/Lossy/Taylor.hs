{-|
module: Numeric.Lossy.Taylor
description: Real functions approximated by Taylor series.
-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Numeric.Lossy.Taylor
  ( expon
  , ln
  , pow
  ) where

import Protolude

-- | Taylor series for the exponential function e^x is \(\sum_{n=0}^{\infty}(x-a)^n/n!\)
expon :: (Fractional a, Real a) => a -> a -> a
expon bound x = sum $ take k [(x - a)^n / fromIntegral (product [1..n]) | n <- [0..]]
  where
    k = fromInteger $ calculateTermsExp bound x
    a = 0

ln :: (Fractional a, Real a) => a -> a -> Maybe a
ln bound x
  | x > 0 = Just $ 2 * (sum $ take k $ (\n -> recip (fromIntegral (2 * n - 1)) * ((x - 1)/(x+1))^(2 * n - 1)) <$> [1..])
  | otherwise = Nothing
  where
    -- TODO: Shouldn't use calculateTermsLog. Taylor series for this function can only converge on 0<x<=2
    -- This gives a wrong number of terms for a given bound
    k = calculateTermsLog bound x

pow :: (Fractional a, Real a) => a -> a -> a -> Maybe a
pow bound x y = expon bound <$> liftA2 (*) (ln bound x) (Just y)

-- Util function needed. Floating values are weird
conv :: Real a => a -> Double
conv = realToFrac

-- | Lagrange formula for the e^x
-- The nth derivative of e^x is e^x
-- Therefore, R_n(x) = \dfrac{e^c}{(x+1)!} x^{n+1}
lagrangeExp :: (Real a, Integral b) => a -> a -> b -> Double
lagrangeExp (conv -> x) (conv -> c) n
  = exp c * ((x - a) ^ (n + 1)) / fromIntegral (product [1..(n+1)])
  where
    a = 0

-- | Given an error bound, calculate number of terms for e^x
calculateTermsExp :: (Real a, Integral b) => a -> a -> b
calculateTermsExp bound x = go 1
  where
    go n = if abs (lagrangeExp x c n) <= realToFrac bound then n else go (n+1)
    c = if x > a then x else a
    a = 0

-- | Lagrange formula for log(x)
-- The nth derivative of log(x) is f^(n+1) = \dfrac{(-1)^{n}}{n!} x^{-n+1}
-- Therefore, R_n(x) = \dfrac{\dfrac{(-1)^n}{n!} c^{-n+1}}{(n+1)!} (x-a)^{n+1}
-- WARNING: This only works for values 0 < x <= 2
lagrangeLog :: (Real a, Integral b) => a -> a -> b -> Double
lagrangeLog (conv -> x) (conv -> c) n
  = ((-1) ^ n * (x - a) ^ (n+1)) / ((c ^ (n-1)) * fromIntegral (nFact * nPlus1Fact))
  where
    nFact = product [1..n]
    nPlus1Fact = nFact * n+1
    a = 1

-- | Given an error bound, calculate number of terms for log(x)
calculateTermsLog :: (Real a, Integral b) => a -> a -> b
calculateTermsLog (realToFrac -> bound) x = go 1
  where
    go n = if abs (lagrangeLog x c n) <= bound then n else go (n+1)
    c = if x > a then x else a
    a = 1
