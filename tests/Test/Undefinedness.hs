{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Test.Undefinedness where

import Protolude

import Algebra.Lattice ((/\), (\/))
import Test.Tasty
import Test.Tasty.QuickCheck

import Language.FCL.AST (Loc())
import Language.FCL.Undefinedness

instance Arbitrary IsInitialized where
  arbitrary = oneof
    [ pure Initialized
    , pure Uninitialized
    , Error <$> (scale (max 3) $ arbitrary @(Set Loc))
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
  [ semilattice testName op bound
  , testProperty "boundedness" $ isBounded op bound
  ]

semilattice
  :: (Show a, Eq a, Arbitrary a)
  => TestName -> (a -> a -> a) -> a -> TestTree
semilattice testName op bound = testGroup testName
  [ testProperty "commutativity" $ commutes op
  , testProperty "associativity" $ associates op
  , testProperty "idempotency" $ idempotates op
  ]

undefinednessTests :: TestTree
undefinednessTests
  = testGroup "Undefinedness tests"
    [ semilattice "IsInitialized is a join semilattice)" (\/) (Error mempty)
    , boundedSemilattice "IsInitialized is a bounded meet semilattice)" (/\) Initialized
    , testProperty "IsInitialized has join-meet-absorption" $ absorbs @IsInitialized (\/) (/\)
    , testProperty "IsInitialized has meet-join-absorption" $ absorbs @IsInitialized (/\) (\/)
    ]
