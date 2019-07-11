{-# options_ghc -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications #-}

module Language.FCL.Orphans () where

import Protolude

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Serialize (Serialize(..))
import qualified Data.Serialize as Serialize
import Test.QuickCheck (Arbitrary(..), NonEmptyList(..))


instance Serialize a => Serialize (NonEmpty a) where
  get = NonEmpty.fromList <$> Serialize.get
  put = Serialize.put . NonEmpty.toList

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = do
    xs <- getNonEmpty <$> arbitrary @(NonEmptyList a)
    case xs of
      (x:xs) -> return (x :| xs)
      _      -> panic "QuickCheck generated an empty list for a NonEmptyList"

  shrink (x :| xs)
    | yss <- shrink (NonEmpty (x:xs))
    = (flip map) yss $ \ys ->
        case getNonEmpty ys of
          (z:zs) -> (z :| zs)
          _      -> panic "QuickCheck shrank a NonEmptyList into an empty list"
