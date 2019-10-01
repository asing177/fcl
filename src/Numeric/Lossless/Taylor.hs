{-|
module: Numeric.Lossless.Taylor
description: Real functions approximated by Taylor series.
-}
module Numeric.Lossless.Taylor (expon, ln, pow) where

import Protolude

expon :: Fractional a => Int -> a -> a
expon p x = sum $ take p [x^n / fromIntegral (product [1..n]) | n <- [0..]]

ln :: Ord a => Fractional a => Int -> a -> Maybe a
ln p x
  | x > 0 = Just $ 2 * (sum $ take p $ [recip (fromIntegral n) * ((x-1)/(x+1))^n | n <- [1,3..]])
  | otherwise = Nothing

pow :: (Fractional b, Ord b) => Int -> b -> b -> Maybe b
pow p x y = expon p <$> liftA2 (*) (ln p x) (Just y)
