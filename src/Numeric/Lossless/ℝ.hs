module Numeric.Lossless.ℝ where

import Protolude
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

newtype ℝ = R {unReal :: NonEmpty Rational}

type ℚ = Rational
type ℚs = NonEmpty Rational

zipWithUnit :: (a -> a -> a) -> a -> [a] -> [a] -> [a]
zipWithUnit f u (x:xs) (y:ys) = f x y : zipWithUnit f u xs ys
zipWithUnit f u (x:xs) []     = f x u : zipWithUnit f u xs []
zipWithUnit f u []     (y:ys) = f u y : zipWithUnit f u [] ys
zipWithUnit _ _ []     []     = []

zipWithUnitNonEmpty :: (a -> a -> a) -> a -> NonEmpty a -> NonEmpty a -> NonEmpty a
zipWithUnitNonEmpty f u xs ys
  = NonEmpty.fromList (zipWithUnit f u (toList xs) (toList ys))

liftOp :: (ℚs -> ℚs -> ℚs) -> ℝ -> ℝ -> ℝ
liftOp f (R xs) (R ys) = R (f xs ys)

withUnit :: (ℚ -> ℚ -> ℚ) -> ℚ -> ℝ -> ℝ -> ℝ
f `withUnit` u = liftOp (zipWithUnitNonEmpty f u)

instance Num ℝ where
  (+) = (+) `withUnit` 0
  (*) = (*) `withUnit` 1
  (-) = (-) `withUnit` 0
  fromInteger n = R (fromInteger n :| [])
