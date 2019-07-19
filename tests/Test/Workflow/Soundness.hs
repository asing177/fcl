module Test.Workflow.Soundness where

import Protolude

import qualified Data.Set as S

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Language.FCL.Pretty

import Test.Workflow.SafeWorkflow.Tests

-- TODO: remove this
import Test.Workflow.SafeWorkflow.Debug

soundnessTests :: TestTree
soundnessTests = testGroup ("Test the soundness checking algorithm on safely constructed workflows")
  [ testGroup "Unit tests"
    [ testGroup "Basic nets" basicNetTests
    , testGroup "Example nets" exampleNetTests
    , testGroup "Witness nets" witnessNetTests
    ]
  , testGroup "QuickCheck tests"
    [ localOption (QuickCheckMaxSize 20) $
      localOption (QuickCheckTests 1000) $
      -- localOption (QuickCheckVerbose True) $
      testProperty "Free choice Petri net based algorithm" $ isSafeWorkflowSound_FreeChoice
    -- , testProperty "General Petri net based algorithm"     (mapSize (const 20)  . withMaxSuccess 100  $ isSafeWorkflowSound_General)

    , localOption (QuickCheckMaxSize 100) $
      localOption (QuickCheckTests 1000) $
      testProperty "Free choice check for extension" isReallyFreeChoice

    -- , testProperty "General cross validation"              (mapSize (const 20)  . withMaxSuccess 5  $ bothYieldSameDecision)

    , localOption (QuickCheckMaxSize 2) $
      localOption (QuickCheckTests 100000) $
      testProperty "Free choice cross validation" bothYieldSameDecisionFC

    , localOption (QuickCheckMaxSize 20) $
      testProperty "Free choice reachability graph is smaller (Safe)" fcGraphIsSmaller_SW

    , localOption (QuickCheckMaxSize 6) $
      testProperty "Free choice reachability graph is smaller (Extended)" fcGraphIsSmaller_ESW

    -- , localOption (QuickCheckMaxSize 4) $
    --   localOption (QuickCheckTests 100) $
    --   testProperty "Just FC" noErrorsFC

    -- , localOption (QuickCheckMaxSize 2) $
    --   localOption (QuickCheckTests 100) $
    --   testProperty "Just general" noErrorsGeneral
    ]
  ]
