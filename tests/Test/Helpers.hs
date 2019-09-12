module Test.Helpers (roundTripTest, roundTripProperty, checkDifference) where

import Protolude

import Data.Algorithm.Diff (getGroupedDiff)
import Data.Algorithm.DiffOutput (ppDiff)
import Data.String (String, lines)

import Test.Tasty.HUnit

type To a b = a -> b
type From b a = b -> Either Text a

roundTripTest
  :: (Show a, Eq a)
  => To a b
  -> From b a
  -> a
  -> Assertion
roundTripTest to from x =
  Right x @=? from (to x)

roundTripProperty
  :: Eq a
  => To a b
  -> From b a
  -> a
  -> Bool
roundTripProperty to from x = Right x == from (to x)

-- | Compares two strings, returns @Nothing@ if they are the same,
-- returns @Just@ the diff if they are different.
checkDifference :: String -> String -> Maybe String
checkDifference exp act = if exp == act
  then Nothing
  else Just $ ppDiff $ getGroupedDiff (lines exp) (lines act)
