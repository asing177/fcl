{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
-- NOTE: This library is developed in a lazy manner. Feel free to add anything if you need it.
module Data.List.List2
  ( List2(..)
  , pattern AsList
  ) where

import Protolude hiding (toList)
import GHC.Exts (IsList(..))

import qualified Data.Foldable as F

data List2 a = List2 a a [a]
  deriving (Eq, Ord, Show)

instance IsList (List2 a) where

  type (Item (List2 a)) = a

  fromList :: [a] -> List2 a
  fromList []       = panic "toList2: The input list is empty"
  fromList [x]      = panic "toList2: The input list is a singleton"
  fromList (x:y:ys) = List2 x y ys

  toList :: List2 a -> [a]
  toList = F.toList

instance Foldable List2 where
  foldMap :: Monoid m => (a -> m) -> List2 a -> m
  foldMap f (List2 x y ys) = f x <> f y <> foldMap f ys

instance Functor List2 where
  fmap :: (a -> b) -> List2 a -> List2 b
  fmap f (List2 x y ys) = List2 (f x) (f y) (map f ys)

-- | Pattern synonym to facilitate pattern matching
-- without turning on `ViewPatterns` at the use-site
pattern AsList :: [a] -> List2 a
pattern AsList l <- (F.toList -> l)
{-# COMPLETE AsList #-}
