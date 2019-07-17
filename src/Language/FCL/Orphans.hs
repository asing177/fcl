{-# options_ghc -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications #-}

module Language.FCL.Orphans () where

import Protolude

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Serialize (Serialize(..))
import qualified Data.Serialize as Serialize
import qualified Test.QuickCheck as QC

instance Serialize a => Serialize (NonEmpty a) where
  get = NonEmpty.fromList <$> Serialize.get
  put = Serialize.put . NonEmpty.toList

instance QC.Arbitrary a => QC.Arbitrary (NonEmpty a) where
  arbitrary = (:|) <$> QC.arbitrary <*> QC.arbitrary
  shrink = QC.genericShrink
