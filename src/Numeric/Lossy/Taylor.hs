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
  , calculateTerms
  , lagrangeFormula
  ) where

import Protolude

-- | Taylor series for the exponential function e^x at a = 0 is \sum_{n=0}^{\infty}x^n/n!
expon :: Fractional a => Int -> a -> a
expon bound x = sum $ take k [x^n / fromIntegral (product [1..n]) | n <- [0..]]
  where
    k = calculateTermsExp bound x

ln :: Real b => Ord a => Fractional a => b -> a -> Maybe a
ln bound x
  | x > 0 = Just $ 2 * (sum $ take p $ [recip (fromIntegral n) * ((x-1)/(x+1))^n | n <- [1,3..]])
  | otherwise = Nothing
  where
    k = calculateTermsLog bound x

pow :: (Real b, Fractional a, Ord a) => b -> a -> a -> Maybe a
pow bound b x = expon bound <$> (liftA2 (*) (ln bound b) (Just x))

-- | Lagrange formula for the particular case of \(b^x = e^{x \log b} at a = 0\).
-- \[
-- R_n(x) = \dfrac{f^{(n+1)}(c)}{(n+1)!} (x-a)^{n+1}
-- As f=e^{x \log b} then f^{(n+1)}(x) = \log^n b e^{x \log b}
-- \]
-- These formulas should be valid latex. Visualise them on https://arachnoid.com/latex/
lagrangeFormula :: (Real a) => a -> a -> a -> Int -> Double
lagrangeFormula (realToFrac -> b) (realToFrac -> x) (realToFrac -> c) n
  = (log b)^n * exp (c * (log b)) * (x ^ (n + 1)) / fromIntegral (product [1..(n+1)])

-- | Given an error bound, calculate number of terms
calculateTermsExp :: (Real a) => a -> a -> a -> Int
calculateTermsExp bound b x = go 0
  where
    go n = if lagrangeFormula b x c n <= realToFrac bound then n else go (n+1)
    c = if b < 0
        then panic "Base b in b^x cannot be lower than 0"
        -- When b > 0. b^x is monotonic
        else if b < 1
             then if x < a then x else a
             else if x < a then a else x
    a = 0 -- a=0 in our implementation of Taylor series

calculateTermsLog :: (Real a) => a -> a -> Int
calculateTermsLog = undefined
