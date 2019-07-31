{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Data.List.List2
  ( module Data.List.List2
  ) where

import Protolude hiding (toList)

data List2 a = List2 a a [a]
  deriving (Eq, Ord, Show)

fromList :: [a] -> List2 a
fromList []       = panic "toList2: The input list is empty"
fromList [x]      = panic "toList2: The input list is a singleton"
fromList (x:y:ys) = List2 x y ys

toList :: List2 a -> [a]
toList (List2 x y ys) = x:y:ys

-- | Pattern synonym to facilitate pattern matching
-- without turning on `ViewPatterns` at the use-site
pattern L2 :: [a] -> List2 a
pattern L2 l <- (toList -> l)
{-# COMPLETE L2 #-}
