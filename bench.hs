#!/usr/bin/env stack
{- stack script
  --resolver lts-13.11
  --package criterion
  --package numeric-quest
  --package numbers
  --extra-dep "numeric-quest-0.2.0.2"
-}

{-# LANGUAGE ScopedTypeVariables #-}

import Criterion.Main
import Data.Number.CReal
import Fraction

main = defaultMain
  [ bgroup ""
      [ bench "CReal" $ nf show $ sqrt (999999999999999999999999999999 * pi :: CReal)
      , bench "Fraction" $ nf show $ (sqrt' (1/999999999999999999999999999999)) (999999999999999999999999999999 * pi' (1/999999999999999999999999999999) :: Fraction)
      ]
  ]
