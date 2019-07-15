module Test.Workflow.Soundness where

import Protolude

import qualified Data.Set as S

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Language.FCL.Pretty

import Test.Workflow.SafeWorkflow.Tests

soundnessTests :: TestTree
soundnessTests = testGroup ("Test the soundness checking algorithm on safely constructed workflows")
  [ testGroup "Unit tests"
    [ testGroup "Basic nets" basicNetTests
    , testGroup "Example nets" exampleNetTests
    ]
  , testGroup "QuickCheck tests"
    [ testProperty "Free choice Petri net based algorithm" (mapSize (const 100) . withMaxSuccess 1000 $ isSafeWorkflowSound_FreeChoice)
    , testProperty "General Petri net based algorithm"     (mapSize (const 20)  . withMaxSuccess 100  $ isSafeWorkflowSound_General)
    , testProperty "Free choice check for extension"       (mapSize (const 100) . withMaxSuccess 1000  $ isReallyFreeChoice)
    -- , testProperty "General cross validation"              (mapSize (const 20)  . withMaxSuccess 5  $ bothYieldSameDecision)
    -- , testProperty "Free choice cross validation"          (mapSize (const 20)  . withMaxSuccess 5  $ bothYieldSameDecisionFC)
    ]
  ]
