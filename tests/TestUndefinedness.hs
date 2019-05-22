{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module TestUndefinedness where

import Protolude

import Algebra.Lattice ((/\), (\/))
import Test.Tasty
import Test.Tasty.QuickCheck

import Language.FCL.Undefinedness

instance Arbitrary IsInitialized where
  arbitrary = oneof . fmap pure
    $ [ Initialized
      , Uninitialized
      , Error mempty
      ]

commutes :: Eq a => (a -> a -> a) -> a -> a -> Bool
commutes op x y
  = (x `op` y) == (y `op` x)

associates :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
associates op x y z
  = (x `op` (y `op` z)) == ((x `op` y) `op` z)

idempotates :: Eq a => (a -> a -> a)
  -> a -> Bool
idempotates op x = x `op` x == x

isIdentity :: Eq a => (a -> a -> a) -> a -> a -> Bool
isIdentity op e x
  = (x `op` e == x) && (e `op` x == x)

isBounded :: Eq a => (a -> a -> a) -> a -> a -> Bool
isBounded op bound x = x `op` bound == x

absorbs
  :: Eq a
  => (a -> a -> a)
  -> (a -> a -> a)
  -> a -> a -> Bool
absorbs op1 op2 a b = a `op1` (a `op2` b) == a

boundedSemilattice
  :: (Show a, Eq a, Arbitrary a)
  => TestName -> (a -> a -> a) -> a -> TestTree
boundedSemilattice testName op bound = testGroup testName
  [ testProperty "commutativity" $ commutes op
  , testProperty "associativity" $ associates op
  , testProperty "idempotency" $ idempotates op
  , testProperty "boundedness" $ isBounded op bound
  ]

undefinednessTests :: TestTree
undefinednessTests
  = testGroup "Undefinedness tests"
    [ boundedSemilattice "IsInitialized is a bounded meet semilattice)" (/\) Initialized
    , boundedSemilattice "IsInitialized is a bounded join semilattice)" (\/) (Error mempty)
    , testProperty "IsInitialized has join-meet-absorption" $ absorbs @IsInitialized (\/) (/\)
    , testProperty "IsInitialized has meet-join-absorption" $ absorbs @IsInitialized (/\) (\/)
    ]
