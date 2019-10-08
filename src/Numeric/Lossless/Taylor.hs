{-|
module: Numeric.Lossless.Taylor
description: Real functions approximated by Taylor series.
-}
module Numeric.Lossless.Taylor (expon, ln, pow) where

import Protolude

-- | Taylor series for the exponential function e^x at a = 0 is \sum_{n=0}^{\infty}x^n/n!
expon :: Fractional a => Int -> a -> a
expon p x = sum $ take p [x^n / fromIntegral (product [1..n]) | n <- [0..]]

ln :: Ord a => Fractional a => Int -> a -> Maybe a
ln p x
  | x > 0 = Just $ 2 * (sum $ take p $ [recip (fromIntegral n) * ((x-1)/(x+1))^n | n <- [1,3..]])
  | otherwise = Nothing

pow :: (Fractional b, Ord b) => Int -> b -> b -> Maybe b
pow p x y = expon p <$> liftA2 (*) (ln p x) (Just y)

-- | Lagrange formula for the particular case of b^x = e^{x \log b} at a = 0
-- R_n(x) = \dfrac{f^{(n+1)}(c)}{(n+1)!} (x-a)^{n+1}
-- As f=e^{x \log b} then f^{(n+1)}(x) = \log^n b e^{x \log b}
lagrangeFormula :: (Floating a) => a -> a -> a -> Int -> a
lagrangeFormula b x c n = (log b)^n * exp (c * (log b)) * (x ^ (n + 1)) / fromIntegral (product [1..(n+1)])

-- | Given an error bound, calculate number of terms
calculateTerms :: (Floating a, Ord a) => a -> a -> a -> Int
calculateTerms bound b x = go 0
  where
    go n = if lagrangeFormula b x c n <= bound then n else go (n+1)
    c = if b < 0
        then panic "Base b in b^x cannot be lower than 0"
        -- When b > 0. b^x is monotonic
        else if b < 1
             then if x < a then a else x
             else if x < a then x else a
    a = 0 -- a=0 in our implementation of Taylor series
