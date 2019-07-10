module TestSoundness where

import Protolude

import qualified Data.Set as S

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Language.FCL.Pretty
import Language.FCL.ReachabilityGraph

import WorkflowGen
import WorkflowGenExamples
import WorkflowGenTests

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

mkSoundnessTest :: SafeWorkflowNet -> [Char] -> TestTree
mkSoundnessTest swf name = testCase name $ do
  let errs = soundnessCheck swf
  assertBool (show . vsep . map ppr $ errs) (null errs)

basicNetTests :: [TestTree]
basicNetTests = map (uncurry mkSoundnessTest) namedBasicNets

exampleNetTests :: [TestTree]
exampleNetTests = map (uncurry mkSoundnessTest) namedExampleNets
