{-# options_ghc -fno-warn-orphans #-}

module Language.FCL.Orphans () where

import Protolude

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Serialize (Serialize(..))
import qualified Data.Serialize as Serialize


instance Serialize a => Serialize (NonEmpty a) where
  get = NonEmpty.fromList <$> Serialize.get
  put = Serialize.put . NonEmpty.toList