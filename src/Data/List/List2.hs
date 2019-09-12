{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-|
Module      : List2
Description : Lists containing at least two elements.

This library is developed in a lazy manner. Feel free to add anything if you need it.
-}
module Data.List.List2
  ( List2(..)
  , pattern AsList
  , fromList
  , reverse
  ) where

import Protolude hiding (toList, reverse)
import GHC.Exts (IsList)

import qualified GHC.Exts as GHC

import qualified Data.List as L (reverse)
import qualified Data.Foldable as F

import Test.QuickCheck

data List2 a = List2 a a [a]
  deriving (Eq, Ord, Show, Generic, NFData, Functor, Foldable, Traversable)

instance IsList (List2 a) where

  type (Item (List2 a)) = a

  fromList :: [a] -> List2 a
  fromList []       = panic "toList2: The input list is empty"
  fromList [x]      = panic "toList2: The input list is a singleton"
  fromList (x:y:ys) = List2 x y ys

  toList :: List2 a -> [a]
  toList = F.toList

instance Arbitrary a => Arbitrary (List2 a) where
  arbitrary = List2 <$> arbitrary <*> arbitrary <*> arbitrary
  shrink    = genericShrink

-- | Pattern synonym to facilitate pattern matching
-- without turning on `ViewPatterns` at the use-site
pattern AsList :: [a] -> List2 a
pattern AsList l <- (F.toList -> l)
{-# COMPLETE AsList #-}

fromList :: [a] -> List2 a
fromList = GHC.fromList

reverse :: List2 a -> List2 a
reverse = GHC.fromList . L.reverse . GHC.toList
