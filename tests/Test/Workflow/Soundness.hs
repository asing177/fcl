module Test.Workflow.Soundness where

import Protolude

import qualified Data.Set as S

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Language.FCL.Pretty
import Language.FCL.ReachabilityGraph

import Test.Workflow.Generation.SafeWorkflowNet.Tests

soundnessTests :: TestTree
soundnessTests = testGroup ("Test the soundness checking algorithm on safely constructed workflows")
  [ testGroup "Unit tests"
    [ testGroup "Basic nets" basicNetTests
    , testGroup "Example nets" exampleNetTests
    ]
  , testGroup "QuickCheck tests"
    [ testProperty "No errors while constructing the reachability graph" (withMaxSuccess 1000 isSafeWorkflowSound)
    ]
  ]
