{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Test.Undefinedness
  ( undefinednessTests
  ) where

import Protolude

import Data.Maybe (fromJust)
import Data.String (String, unlines)
import qualified Data.Set as S

import Algebra.Lattice ((/\), (\/))

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden (findByExtension)
import Test.Tasty.QuickCheck

import Language.FCL.AST (Loc(), Script)
import Language.FCL.Pretty (ppr)
import Language.FCL.Parser (parseFile)
import Language.FCL.Undefinedness (IsInitialized(..), InvalidStackTrace, undefinednessAnalysis, fastUndefinednessAnalysis)

import Test.Helpers (checkDifference)

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

-- TODO: Reenable this once we can generate type-safe FCL scripts.
-- fastAggreesWithGeneral :: Script -> Bool
-- fastAggreesWithGeneral script = case (genRes, fastRes) of
--   (Right _, Nothing) -> True
--   (Left allInvalidTraces, Just invalidTrace) -> invalidTrace `elem` allInvalidTraces
--   _ -> False
--   where genRes  = undefinednessAnalysis script
--         fastRes = fastUndefinednessAnalysis script

undefinednessTests :: IO TestTree
undefinednessTests = do
  -- it is run for all positive inputs
  positivePaths <- findByExtension [".s"] "tests/script/positive"
  -- it is run for only negative undefinedness inputs (others might not even parse/type-check)
  negativePaths <- findByExtension [".s"] "tests/script/negative/undefinedness"
  let positiveUnitTests = map mkUndefinednessTest positivePaths
      negativeUnitTests = map mkUndefinednessTest negativePaths
  return $ testGroup "Undefinedness tests"
    [ testGroup "Unit tests"
      [ testGroup "Positive (both accept)" positiveUnitTests
      , testGroup "Negative (both reject - error given by fast is found by general)" negativeUnitTests
      ]
    , testGroup "QuickCheck tests"
        [ semilattice "IsInitialized is a join semilattice" (\/) (Error mempty)
        , boundedSemilattice "IsInitialized is a bounded meet semilattice" (/\) Initialized
        , testProperty "IsInitialized has join-meet-absorption" $ absorbs @IsInitialized (\/) (/\)
        , testProperty "IsInitialized has meet-join-absorption" $ absorbs @IsInitialized (/\) (\/)
        ]
    ]

-- | Checks whether the fast and general algorithms give
-- the same results for a given input. Furthermore, if they both reject it the
-- error given by the fast algorithm is found by the general one too.
mkUndefinednessTest :: FilePath -> TestTree
mkUndefinednessTest path = testCase path $ do
  ast <- parseFile path
  let genRes  = undefinednessAnalysis ast
      fastRes = fastUndefinednessAnalysis ast
  case (genRes, fastRes) of
    (Right _, Nothing) -> return ()
    (Left allInvalidTraces, Just invalidTrace) ->
      assertBool (chkDiff invalidTrace allInvalidTraces) (invalidTrace `elem` allInvalidTraces)
    (Right _, Just invalidTrace) -> assertFailure $ unlines
      [ "The general algorithm accepted the input, but the fast one rejected it with the following error:"
      , show $ ppr $ invalidTrace
      ]
    (Left allInvalidTraces, Nothing) -> assertFailure $ unlines
      [ "The fast algorithm accepted the input, but the general one rejected it with the following error(s):"
      , show $ ppr $ allInvalidTraces
      ]
  where
    chkDiff :: InvalidStackTrace -> [InvalidStackTrace] -> String
    chkDiff x y = unlines
      [ "Both algorirthms rejected the input, but the error found by the fast algorithm is not found by the general one."
      , "The general algorithms's output (<) and the fast algorithm's output (>):"
      , fromJust $ checkDifference (show x) (show y)
      ]
