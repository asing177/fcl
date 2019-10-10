{-|
module: Numeric.Lossy.Taylor
description: Real functions approximated by Taylor series.
-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Numeric.Lossy.Taylor
  ( pow
  ) where

import Protolude
import Fraction (Transcendental(..), Fraction)

-- | a^b for fractions
pow :: (Num a, Transcendental a) => Fraction -> a -> a -> a
pow bound a b = exp' bound (log' bound a * b)
