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
    [ testGroup "Basic nets"   basicNetTests
    , testGroup "Example nets" exampleNetTests
    , testGroup "Some arbitrary nets" someArbitraryNetTests
    , testGroup "Cross validation witness nets" crossValidWitnessNetTests
    , testGroup "Sound but not safe witness nets" soundButNotSafeWitnessNetTests
    ]

  , localOption (QuickCheckMaxSize 20) $
    localOption (QuickCheckTests 1000) $
    testGroup "QuickCheck tests"
    [ testProperty "Number of transitions in a generated workflow is LEQ to its size" $ transitionsLEQSize

    , testProperty "Split-and-Merge on safe workflows" $ isSafeWorkflowSound_SplitAndMerge

    , testProperty "General Petri net based soundness check on safe workflows" isSafeWorkflowSound_General

    , testProperty "Extended safe workflow is still free choice" isReallyFreeChoice

    , testProperty "Split-and-Merge reachability graph is smaller (safe)" samGraphIsSmaller_SW

    , localOption (QuickCheckMaxSize 6) $
      localOption (QuickCheckTests 10000) $
      testProperty "Split-and-Merge is stricter (basic)" splitAndMergeIsStricter

    , localOption (QuickCheckMaxSize 12) $
      localOption (QuickCheckTests 100) $
      testProperty "Split-and-Merge is stricter (complex)" splitAndMergeIsStricter

    -- FIXME: Pending, because the general algorithm does not generate certain states in case of local loops
    -- , localOption (QuickCheckMaxSize 6) $
    --   testProperty "Free choice reachability graph is smaller (Extended)" samGraphIsSmaller_ESW
    ]

  , localOption (QuickCheckMaxSize 20) $
    testGroup "Transition structuring"
    [ testProperty "Structure/unstructure is id (Safe)" $ structureUnstructureId_Safe
    , testProperty "Structure/unstructure is id (Extended)" $ structureUnstructureId_Extended
    , testProperty "Simple structure/unstructure is id (Safe)" $ structureUnstructureSimplyId_Safe
    , testProperty "Simple structure/unstructure is id (Extended)" $ structureUnstructureSimplyId_Extended
    ]
  ]
